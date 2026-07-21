;;; ndl-search.el --- ndl-search  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Taro Sato
;;
;; Author: Taro Sato <okomestudio@gmail.com>
;; URL: https://github.com/okomestudio/ndl-search.el
;; Version: 0.1.1
;; Keywords: convenience
;; Package-Requires: ((emacs "30.1") (s "1.13.1"))
;;
;;; License:
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; A search utility for the National Diet Library, Japan (国立国会図書館).
;;
;;; Code:

(require 'cl-lib)
(require 'dom)
(require 'map)
(require 'url-expand)
(require 'url-parse)
(require 'url-util)
(require 'xml)

(require 's)

(defgroup ndl-search nil
  "Customization group for `ndl-search'."
  :group 'convenience
  :prefix "ndl-search-")

(defcustom ndl-search-sleep '(0.10 0.15)
  "Sleep in seconds between HTTP calls."
  :type '(choice
          (integer :tag "Sleep in seconds")
          (cons (integer :tag "Sleep in seconds")
                (integer :tag "Jitter in seconds")))
  :group 'ndl-search)

(defcustom ndl-search-max-items 400
  "Maximum items to get from the NDL search API."
  :type '(integer :tag "Max item count")
  :group 'ndl-search)

(defcustom ndl-search-item-types-abbrev
  '(("紙" . "")
    ("記録メディア" . "")
    ("デジタル" . "󰓷")
    ("マイクロ" . "󰍉"))
  "Item types and their abbreviations."
  :type '(repeat (cons (string :tag "資料形態")
                       (string :tag "省略形")))
  :group 'ndl-search)

(defcustom ndl-search-item-material-types-abbrev
  '(("図書" . "図")
    ("電子書籍・電子雑誌" . "電")
    ("録音資料" . "録")
    ("雑誌" . "雑")
    ("雑誌タイトル" . "雑タ")
    ("記事" . "記")
    ("児童書" . "児")
    ("映像資料" . "映")
    ("博士論文" . "博"))
  "Item material types and their abbreviations."
  :type '(repeat (cons (string :tag "種類種別")
                       (string :tag "省略形")))
  :group 'ndl-search)

;; Bibilography Item

(defconst ndl-search--field-processors
  '(("出版事項" . ndl-search--process-publisher)
    ("出版年月日等" . ndl-search--process-publication-date)
    ("出版年（W3CDTF）" . ndl-search--process-publication-year)
    ("数量" . ndl-search--process-quantity)
    ("著者・編者" . ndl-search--process-authors)
    ("シリーズ著者・編者" . ndl-search--process-authors)
    ("著者標目" . ndl-search--process-author-indices)
    ("書誌ID（NDLBibID）" . ndl-search--process-ndl-bib-id)))

(defun ndl-search-bib-item-get (url)
  "Get bib item as an alist from URL.
The bib item URL should have a path '/books/<id>'."
  (let ((url-automatic-caching t)
        bib-item)
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (when-let* ((dom (libxml-parse-html-region (point) (point-max))))
        (mapcar
         (lambda (node)
           (when-let*
               ((field (dom-inner-text (dom-by-tag node 'dt)))
                (value (if-let* ((proc (map-elt ndl-search--field-processors field)))
                           (funcall proc (car (dom-by-tag node 'dd)))
                         (dom-inner-text (dom-by-tag node 'dd)))))
             (push (cons field value) bib-item)))
         (dom-by-class
          (car (dom-by-class dom "pages-books-section-bib-list"))
          "pages-books-ndls-section-bib-list-item"))))
    (if bib-item
        (progn
          (push (cons "ndl:url" url) bib-item)
          (pp bib-item))
      (message "No bib item extracted from %s" url))
    bib-item))

(defun ndl-search--process-authors (node)
  "Process NODE ('dd') as author/editor/contributer info alist."
  (apply #'append
         (mapcar
          (lambda (span)
            (let ((s (dom-inner-text span)))
              (when (string-match "\\`\\(.*?\\)\\s-+\\(\\S-+\\)\\'" s)
                (let ((role (match-string 2 s))
                      (fullnames (string-split (match-string 1 s) ", ")))
                  (mapcar (lambda (fullname)
                            (delq nil
                                  (list (when fullname (cons "姓名" fullname))
                                        (when role (cons "区分" role)))))
                          fullnames)))))
          (dom-by-tag node 'span))))

(defun ndl-search--process-author-indices (node)
  "Process NODE ('dd') as author indices alist."
  (let ((pattern
         (concat
          "^\\s-*"
          "\\(\\(?18:[^ ：:]+\\) *[：:] *\\)?"
          "\\(\\(?2:[^,]+\\)\\(, *\\(?4:[^, ]+\\)\\)\\)" ; surname, given name
          "\\(, *\\(\\(?6:[0-9]+\\)-\\(?7:[0-9]+\\|.+\\)?\\|.+\\)\\)?" ; year-of-birth, year-of-death
          "\\s-+"
          "\\(\\(?9:\\cK+\\)\\(, *\\(?11:\\cK+\\)\\)\\)"
          "\\(, *\\(\\([0-9]+\\)-\\([0-9]+\\|.+\\)?\\|.+\\)\\)?"
          "\\( +( *\\(?16:[0-9]+\\) *)\\)?.*" ; entity-id
          "?")))
    (mapcar
     (lambda (span)
       (let ((s (dom-inner-text span)))
         (when (string-match pattern s)
           (let ((role (match-string 18 s))
                 (surname (match-string 2 s))
                 (given-name (match-string 4 s))
                 (yob (match-string 6 s))
                 (yod (match-string 7 s))
                 (surname-kana (match-string 9 s))
                 (given-name-kana (match-string 11 s))
                 (entity-id (match-string 16 s)))
             (delq nil
                   (list
                    (when role (cons "区分" role))
                    (when surname (cons "氏" surname))
                    (when given-name (cons "名" given-name))
                    (when yob (cons "生年" yob))
                    (when yod (cons "没年" yod))
                    (when surname-kana (cons "ヨミカタ／氏" surname-kana))
                    (when given-name-kana (cons "ヨミカタ／名" given-name-kana))
                    (when entity-id (cons "ID" entity-id))))))))
     (dom-by-tag node 'span))))

(defun ndl-search--process-publication-date (node)
  "Process NODE ('dd') as publication date."
  (let ((pattern "\\([0-9]+\\)\\(\\.\\([0-9]+\\)\\)?\\(\\.\\([0-9]+\\)\\)?")
        (s (dom-inner-text (dom-by-tag node 'span))))
    (if (string-match pattern s)
        (let ((year (match-string 1 s))
              (month (match-string 3 s))
              (day (match-string 5 s)))
          (list (when year (string-to-number year))
                (when month (string-to-number month))
                (when day (string-to-number day))))
      s)))

(defun ndl-search--process-publication-year (node)
  "Process NODE ('dd') as publication year."
  (let ((pattern "\\([0-9]+\\)")
        (s (dom-inner-text (dom-by-tag node 'span))))
    (if (string-match pattern s)
        (let ((year (match-string 1 s)))
          (string-to-number year))
      s)))

(defun ndl-search--process-publisher (node)
  "Process NODE ('dd') as publisher info alist."
  (let ((pattern "^\\(\\([^ :]+\\) *: *\\)?\\([^ (]+\\)\\( *(\\([^)]+\\))\\)?"))
    (mapcar
     (lambda (a)
       (let ((s (dom-inner-text a)))
         (when (string-match pattern s)
           (let ((place (match-string 2 s))
                 (publisher (match-string 3 s))
                 (role (match-string 5 s)))
             (delq nil
                   (list
                    (when place (cons "所在地" place))
                    (when publisher (cons "出版社" publisher))
                    (when role (cons "その他" role))))))))
     (dom-by-tag node 'a))))

(defun ndl-search--process-quantity (node)
  "Process NODE ('dd') as quantity."
  (let ((pattern "^\\([0-9]+\\) *\\(.+\\)?$")
        (s (dom-inner-text (dom-by-tag node 'span))))
    (if (string-match pattern s)
        (let ((quantity (match-string 1 s))
              (unit (match-string 2 s)))
          (list (cons "数量" (string-to-number quantity))
                (cons "単位" unit)))
      s)))

(defun ndl-search--process-ndl-bib-id (node)
  "Process NODE ('dd') as quantity."
  (delq nil
        (list (when-let* ((span (dom-by-tag node 'span)))
                (cons "NDLBibID" (dom-inner-text span)))
              (when-let* ((a (dom-by-tag node 'a)))
                (cons "URL" (dom-inner-text a))))))

;; Search Query

(defun ndl-search--extract-search-items (url)
  "Extract items from search query result at URL."
  (message "Querying URL: %s" url)
  (cl-letf* (((symbol-function 'cname)
              (lambda (s)
                (concat "\\(?:^\\|[[:space:]]+\\)"
                        (regexp-quote s)
                        "\\(?:[[:space:]]+\\|$\\)")))
             ((symbol-function 'by-class)
              (lambda (dom class)
                (dom-by-class dom (cname class))))
             ((symbol-function 'inner-text)
              (lambda (node)
                (when node
                  (dom-inner-text node)))))
    (let* ((url-request-method "GET")
           (url-request-data nil)
           (url-request-extra-headers nil)
           (url-automatic-caching t)
           (response-buffer (url-retrieve-synchronously url)))
      (unless response-buffer
        (error "Response not received from %s" url))
      (with-current-buffer response-buffer
        (set-buffer-multibyte t)
        (decode-coding-region (point-min) (point-max) 'utf-8)
        (goto-char (point-min))
        (search-forward "\n\n" nil t)
        (let ((dom (libxml-parse-html-region (point) (point-max))))
          (cons
           (dom-attr (car (dom-by-id dom (cname "layouts-global-skip-link")))
                     'href)
           (mapcar
            (lambda (node)
              (let ((item-types
                     (mapconcat
                      (lambda (span)
                        (let ((s (dom-inner-text span)))
                          (map-elt ndl-search-item-types-abbrev s s)))
                      (dom-by-class node "search-result-item-type-tag")
                      " "))
                    (item-material-types
                     (mapconcat
                      (lambda (span)
                        (let ((s (dom-inner-text span)))
                          (map-elt ndl-search-item-material-types-abbrev s s)))
                      (dom-by-class node "search-result-item-material-type-tag")
                      "/"))
                    (meta (car (by-class node "search-result-item-meta"))))
                (list
                 (cons 'title
                       (inner-text (car (by-class node "search-result-item-heading"))))
                 (cons 'categories (concat item-types))
                 (cons 'material-types item-material-types)
                 (cons 'url
                       (dom-attr
                        (car (dom-by-tag
                              (car (by-class node "base-heading"))
                              'a))
                        'href))
                 (cons 'author
                       (inner-text (car (by-class meta "author"))))
                 (cons 'publisher
                       (inner-text (car (by-class meta "publisher"))))
                 (cons 'publish-date
                       (inner-text (car (by-class meta "publish-date"))))
                 (cons 'book
                       (inner-text (car (by-class meta "book"))))
                 (cons 'book-publish-date
                       (inner-text (car (by-class meta "book-publish-date"))))
                 (cons 'page
                       (inner-text (car (by-class meta "page"))))
                 (cons 'highlight
                       (mapconcat (lambda (ul)
                                    (inner-text ul))
                                  (by-class node "search-result-item-highlight")
                                  "\n"))
                 ;; TODO(2026-07-19): These nodes are filled dynamically
                 ;; via JS and not available; consider a headless
                 ;; browser like puppeteer or playwright to support this:
                 ;; (cons 'children
                 ;;       (inner-text
                 ;;        (car (by-class meta "search-result-item-children"))))
                 )))
            (by-class (car (by-class dom "search-result-body"))
                      "search-result-item"))))))))

(cl-defun ndl-search-query (&key any title creator (dpid "iss-ndl-opac"))
  "Make an OpenURL search query.
ANY maps to the query parameter 'any'.
TITLE maps to the query parameter 'btitle'.
CREATOR maps to the query parameter 'au'."
  (let* ((query-params
          (delq nil (list
                     (when dpid (cons 'ndl_dpid (list dpid)))
                     (when any (cons 'any (list any)))
                     (when title (cons 'btitle (list title)))
                     (when creator (cons 'au (list creator))))))
         (url (ndl-search-build-url
               "https://ndlsearch.ndl.go.jp/api/openurl" nil query-params))
         all-items)
    (while
        (pcase-let* ((`(,resolved-url . ,items) (ndl-search--extract-search-items url))
                     (parts (split-string
                             (url-filename
                              (url-generic-parse-url resolved-url))
                             "\\?"))
                     (url-path (nth 0 parts))
                     (raw-query (nth 1 parts))
                     (query-params (when raw-query
                                     (url-parse-query-string raw-query))))
          (message "Extracted %d item(s) from '%s'" (length items) resolved-url)
          (when items
            (setq all-items (append all-items items))
            (unless (<= ndl-search-max-items (length all-items))
              ;; Paginate
              (let* ((from (string-to-number
                            (car (map-elt query-params "from" (list "0")))))
                     (size (string-to-number
                            (car (map-elt query-params "size" (list "20"))))))
                (setf (map-elt query-params "from")
                      (list (number-to-string (+ from size))))
                (setf (map-elt query-params "size")
                      (list "100")))

              (setq url (url-recreate-url
                         (url-parse-make-urlobj
                          "https" nil nil "ndlsearch.ndl.go.jp" nil
                          (concat url-path
                                  "?" (url-build-query-string query-params))
                          nil nil t)))

              ;; Be nice to the API server.
              (apply #'ndl-search--sleep
                     (if (listp ndl-search-sleep) ndl-search-sleep (list ndl-search-sleep)))
              t))))
    (ndl-search--completing-read (take ndl-search-max-items all-items))))

(defun ndl-search--affixation-function (completions item-alist-getter)
  "Affixation function for completion.
COMPLETIONS are passed from `completing-read'.
ITEM-ALIST-GETTER get an item alist for one of the COMPLETIONS."
  (cl-letf
      (((symbol-function 's-align)
        (lambda (text column &optional side)
          "Align TEXT to the COLUMN by SIDE."
          (apply #'concat
                 (let ((col (pcase side
                              ('right (- column (string-width text)))
                              (_ column))))
                   (list (propertize " " 'display `(space :align-to ,col))
                         text))))))
    (mapcar
     (lambda (completion)
       (let-alist (funcall item-alist-getter completion)
         (let* ((width (window-body-width (active-minibuffer-window)))
                (prefix
                 (concat (s-align (or .categories "") 9 'right)))
                (main
                 (concat (s-align (or .title "") 11)
                         (s-align (or .author "") (round (* width 0.45)))
                         (s-align (or .publisher .book)
                                  (round (* width 0.65)))
                         (s-align (or .publish-date
                                      (concat .book-publish-date
                                              ": "
                                              .page))
                                  (round (* width 0.80)))))
                (suffix
                 (concat (s-align (or .material-types "") width 'right))))
           (list main prefix suffix))))
     completions)))

(defun ndl-search--completing-read (search-result-items)
  "Completing read SEARCH-RESULT-ITEMS."
  (let ((entity-path
         (cond
          ((featurep 'consult)
           (ndl-search--completing-read-consult search-result-items))
          (t
           (let ((completion-extra-properties
                  '(:affixation-function
                    (lambda (completions)
                      (ndl-search--affixation-function
                       completions
                       (lambda (completion)
                         (get-text-property 0 'item-data completion)))))))
             (completing-read "Filter: "
                              (mapcar (lambda (it)
                                        (propertize (alist-get 'url it)
                                                    'item-data it))
                                      search-result-items)))))))
    (url-recreate-url
     (url-parse-make-urlobj
      "https" nil nil "ndlsearch.ndl.go.jp" nil entity-path nil nil t))))

(defun ndl-search--completing-read-consult (search-result-items)
  "Completing read SEARCH-RESULT-ITEMS using `consult'."
  (let* ((candidates
          (mapcar (lambda (it)
                    (cons (propertize (alist-get 'url it) 'item-data it)
                          it))
                  search-result-items))
         (completion-extra-properties
          '(:affixation-function
            (lambda (completions)
              (ndl-search--affixation-function
               completions
               (lambda (completion)
                 (get-text-property 0 'item-data completion))))))
         (preview-buf (get-buffer-create " *ndl-search-preview*"))
         (preview-win nil))
    (car
     (consult--read
      candidates
      :prompt "Filter (consult): "
      :lookup #'consult--lookup-cons
      :inherit-input-method t
      :preview-key 'any
      :state
      (lambda (action candidate)
        (pcase action
          ('preview
           (when candidate
             (let ((content ""))
               (when-let* ((item-data (cdr candidate)))
                 (let-alist item-data
                   (setq content (concat (or .highlight "")
                                         (or .children "")))))

               (with-current-buffer preview-buf
                 (let ((inhibit-read-only t))
                   (erase-buffer)
                   (when content
                     (insert content))
                   (special-mode)))

               (if (and (window-live-p preview-win)
                        (eq (window-buffer preview-win) preview-buf))
                   (when (string-empty-p content)
                     (delete-window preview-win)
                     (setq preview-win nil))
                 (unless (string-empty-p content)
                   (setq preview-win
                         (display-buffer preview-buf
                                         '((display-buffer-below-selected
                                            display-buffer-at-bottom)
                                           (window-height . 0.2)))))))))))))))

;; Interactive Commands

;;;###autoload
(defun ndl-search-any (query)
  "Perform 'any' search for QUERY."
  (interactive "sNDL search (any): ")
  (when-let* ((url (ndl-search-query :any query)))
    (ndl-search-bib-item-get url)))

;;;###autoload
(defun ndl-search-creator (query)
  "Perform 'creator' search for QUERY."
  (interactive "sNDL search (creator): ")
  (when-let* ((url (ndl-search-query :creator query)))
    (ndl-search-bib-item-get url)))

;;;###autoload
(defun ndl-search-title (query)
  "Perform 'title' search for QUERY."
  (interactive "sNDL search (title): ")
  (when-let* ((url (ndl-search-query :title query)))
    (ndl-search-bib-item-get url)))

;; Utilities

(defun ndl-search-build-url (base-url path-segments query-alist)
  "Construct a safe, fully-encoded URL.
BASE-URL is the endpoint root (e.g., 'https://example.com/api/').
PATH-SEGMENTS is a list of unencoded string directories or endpoints.
QUERY-ALIST is an association list of keys and values for parameters."
  (let* ((clean-path (mapconcat #'url-hexify-string path-segments "/"))
         (full-url (url-expand-file-name clean-path base-url)))
    (if query-alist
        (concat full-url "?" (url-build-query-string query-alist))
      full-url)))

(defun ndl-search--sleep (seconds &optional jitter)
  "Sleep for SECONDS plus a JITTER.
When given, JITTER in seconds will be used to generate a random number
betwee 0 and JITTER."
  (sleep-for (+ seconds (if jitter
                            (/ (random (round (* jitter 1000.0))) 1000.0)
                          0))))

(provide 'ndl-search)
;;; ndl-search.el ends here
