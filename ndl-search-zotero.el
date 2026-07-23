;;; ndl-search-zotero.el --- Zotero integration for ndl-search  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2026 Taro Sato
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
;; This module provides a Zotero integration for `ndl-search'.
;;
;; References:
;;
;;   - Zotero API schema: https://api.zotero.org/schema
;;
;;; Code:

(require 'map)

(require 'ndl-search)
(require 'zotero)

;;;###autoload
(defun ndl-search-zotero-create-item ()
  "Create a Zotero item obtained from QUERY."
  (interactive)
  (when-let* ((item (call-interactively #'ndl-search-any)))
    (let ((json (pcase (map-elt item "資料種別")
                  ("記事" (ndl-search-zotero-article--create item))
                  ("図書" (ndl-search-zotero-book--create item))
                  (_ (ndl-search-zotero-book--create item)))))
      (when ndl-search-debug
        (pp json))
      (zotero-create-item json))))

(defun ndl-search-zotero-article--create (item)
  "Create an article from ITEM."
  (let ((creator-indices (ndl-search-zotero--creator-indices (map-elt item "著者標目"))))
    (append
     (list :itemType "magazineArticle")
     (ndl-search-zotero--json-title (map-elt item "タイトル")
                                    (map-elt item "シリーズタイトル"))
     (ndl-search-zotero--json-creators (map-elt item "著者・編者")
                                       (map-elt item "シリーズ著者・編者")
                                       creator-indices)
     (list :publicationTitle (map-elt item "タイトル（掲載誌）"))
     (ndl-search-zotero--json-publisher (map-elt item "出版事項（掲載誌）"))
     (ndl-search-zotero--json-date (map-elt item "掲載年月日（W3CDTF）"))
     (list :volume (map-elt item "掲載巻")
           :issue (map-elt item "掲載号")
           :pages (map-elt item "掲載ページ"))
     (ndl-search-zotero--json-language (map-elt item "本文の言語コード"))
     (ndl-search-zotero--json-extra
      (list (cons "NDLBibID"
                  (map-elt (map-elt item "書誌ID（NDLBibID）") "NDLBibID")))))))

(defun ndl-search-zotero-book--create (item)
  "Create a book from ITEM."
  (let ((creator-indices (ndl-search-zotero--creator-indices (map-elt item "著者標目"))))
    (append
     (list :itemType "book")
     (ndl-search-zotero--json-title (map-elt item "タイトル"))
     (when-let* ((volume (map-elt item "巻次・部編番号")))
       (list :volume volume))
     (ndl-search-zotero--json-creators (map-elt item "著者・編者")
                                       (map-elt item "シリーズ著者・編者")
                                       creator-indices)
     (when-let* ((s (map-elt item "シリーズタイトル")))
       (let ((parts (when (string-match "[ \t]*[;][ \t]*" s)
                      (cons (substring s 0 (match-beginning 0))
                            (substring s (match-end 0))))))
         (list :series (or (car parts) s)
               :seriesNumber (cdr parts))))
     (list :edition (map-elt item "版"))
     (ndl-search-zotero--json-publisher (map-elt item "出版事項"))
     (ndl-search-zotero--json-date (map-elt item "出版年月日等"))
     (list :numPages (let ((it (map-elt item "数量")))
                       (if (string= (map-elt it "単位") "p")
                           (map-elt it "数量")))
           :isbn (map-elt item "ISBN"))
     (ndl-search-zotero--json-language (map-elt item "本文の言語コード"))
     (ndl-search-zotero--json-tags (map-elt item "NDC10版")
                                   (map-elt item "件名標目"))
     (ndl-search-zotero--json-extra
      (list (cons "NDLBibID"
                  (map-elt (map-elt item "書誌ID（NDLBibID）") "NDLBibID")))))))

(defun ndl-search-zotero--creator-indices (item)
  "Turn creator indices (著者標目) ITEM into an alist.
The alist maps full names to '(cons surname given-name)'. The alist is
typically used to infer surname and given name from a full name."
  (mapcar (lambda (it)
            (let ((surname (map-elt it "氏"))
                  (given-name (map-elt it "名")))
              (cons (concat surname given-name)
                    (cons surname given-name))))
          item))

(defun ndl-search-zotero--json-tags (ncd10 topic-term-indices)
  "Render NCD10 and TOPIC-TERM-INDICES as :tags."
  (list :tags
        (vconcat
         (mapcar
          (lambda (tag) (list :tag tag))
          (flatten-list
           (append
            (mapcar (lambda (s)
                      (when-let*
                          ((_ (string-match "\\`.*: *\\(?1:.+\\)\\'" s))
                           (terms (string-split (match-string 1 s)
                                                "[．]" t "\\s-+")))
                        terms))
                    (list ncd10))
            (mapcar (lambda (it)
                      (map-elt it "件名"))
                    topic-term-indices)))))))

(defun ndl-search-zotero--json-publisher (publisher-items)
  "Render PUBLISHER-ITEMS as :publisher and :place."
  (let ((it (seq-find (lambda (it)
                        (let ((etc (map-elt it "その他")))
                          (or (null etc) (string= etc "出版"))))
                      publisher-items)))
    (list :publisher (map-elt it "出版社")
          :place (map-elt it "所在地"))))

(defun ndl-search-zotero--json-title (title &optional series-title)
  "Render TITLE and SERIES-TITLE as :title and :shortTitle."
  (let* ((parts (string-split title "[:：]+" 'omit-empty "\\s-+"))
         (short-title (car parts))
         (title (string-join parts " ")))
    (append (list :title (concat title
                                 (if series-title
                                     (concat " （" series-title "）")
                                   "")))
            (when (< (length short-title) (length title))
              (list :shortTitle short-title)))))

(defun ndl-search-zotero--json-creators (creators series-creators &optional creator-indices)
  "Render CREATORS and SERIES-CREATORS as :creators.
When given, CREATOR-INDICES holds creator index (著者標目) entries."
  (list :creators
        (vconcat
         (mapcar
          (lambda (creator)
            (append
             (list :creatorType
                   (pcase (map-elt creator "区分")
                     ("著" "author")
                     ("編" (if (map-elt creator "シリーズ")
                               "seriesEditor"
                             "editor"))
                     ("訳" "translator")
                     ("監修" "editor")
                     (_
                      (ndl-search--message "Unknown role for '%s': '%s'"
                                           (or (map-elt creator "氏名")
                                               (concat (map-elt creator "氏")
                                                       (map-elt creator "名")))
                                           (map-elt creator "区分"))
                      "contributor")))
             (if-let* ((fullname (map-elt creator "氏名")))
                 (if-let* ((index (map-elt creator-indices fullname))
                           (surname (car index))
                           (given-name (cdr index)))
                     (list :lastName surname
                           :firstName given-name)
                   (list :name fullname))
               (list :lastName (map-elt creator "氏")
                     :firstName (map-elt creator "名")))))
          (append creators
                  (mapcar (lambda (it)
                            (push (cons "シリーズ") it)
                            ;; (setf (map-elt it "区分")
                            ;;       (concat "シリーズ" (map-elt it "区分")))
                            it)
                          series-creators))))))

(defun ndl-search-zotero--json-date (date)
  "Render DATE as :date.
If DATE is a list of form '(year month day)', it will be rendered as
'YYYY-MM-DD'."
  (list :date
        (if (listp date)
            (cond
             ((nth 2 date)
              (format "%04d-%02d-%02d" (nth 0 date) (nth 1 date) (nth 2 date)))
             ((nth 1 date)
              (format "%04d-%02d" (nth 0 date) (nth 1 date)))
             (t
              (format "%04d" (nth 0 date))))
          date)))

(defun ndl-search-zotero--json-language (lang)
  "Render LANG as :language."
  (list :language (pcase lang
                    ("jpn" "ja")
                    ("eng" "en")
                    (_ lang))))

(defun ndl-search-zotero--json-extra (alis)
  "Render key value pairs in ALIS as :extra."
  (list :extra (mapconcat (lambda (it)
                            (format "%s: %s" (car it) (cdr it)))
                          alis
                          "\n")))

(provide 'ndl-search-zotero)
;;; ndl-search-zotero.el ends here
