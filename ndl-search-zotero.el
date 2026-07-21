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
;; For the Zotero API schema, see https://api.zotero.org/schema.
;;
;;; Code:

(require 'map)

(require 'ndl-search)
(require 'zotero)

;;;###autoload
(defun ndl-search-zotero-create-item (query)
  "Create a Zotero item obtained from QUERY."
  (interactive "sNDL search (any): ")
  (when-let* ((item (ndl-search-any query)))
    (let ((json (pcase (map-elt item "資料種別")
                  (_ (ndl-search-zotero-book--create item)))))
      (zotero-create-item json))))

(defun ndl-search-zotero-book--create (item)
  "Create a book ITEM in JSON."
  (let ((creator-indices
         (mapcar (lambda (it)
                   (cons (concat (map-elt it "氏") (map-elt it "名"))
                         (cons (map-elt it "氏") (map-elt it "名"))))
                 (map-elt item "著者標目"))))
    (append
     (list :itemType "book")
     (when-let* ((title-parts (string-split (map-elt item "タイトル")
                                            "[:：]+" 'omit-empty "\\s-+"))
                 (title (string-join title-parts " "))
                 (short-title (car title-parts)))
       (append
        (list :title title)
        (when (< (length short-title) (length title))
          (list :shortTitle short-title))))
     (list :creators
           (vconcat
            (mapcar (lambda (it)
                      (ndl-search-zotero--transform-creator it creator-indices))
                    (append
                     (map-elt item "著者・編者")
                     (mapcar (lambda (it)
                               (setf (map-elt it "区分")
                                     (concat "シリーズ" (map-elt it "区分")))
                               it)
                             (map-elt item "シリーズ著者・編者"))))))
     (list :series (map-elt item "シリーズタイトル")
           :seriesNumber nil
           :edition (map-elt item "版"))
     (when-let*
         ((it (seq-find (lambda (el)
                          (or (null (map-elt el "その他"))
                              (string= (map-elt el "その他") "出版")))
                        (map-elt item "出版事項"))))
       (list :publisher (map-elt it "出版社")
             :place (map-elt it "所在地")))
     (list :date (ndl-search-zotero--transform-date (map-elt item "出版年月日等"))
           :numPages (let ((it (map-elt item "数量")))
                       (if (string= (map-elt it "単位") "p")
                           (map-elt it "数量")))
           :isbn (map-elt item "ISBN")
           :language (ndl-search-zotero--transform-language (map-elt item "本文の言語コード")))
     (list :extra
           (mapconcat
            (lambda (it)
              (format "%s: %s" (car it) (cdr it)))
            (append
             (when-let* ((id (map-elt (map-elt item "書誌ID（NDLBibID）")
                                      "NDLBibID")))
               (list (cons "NDLBibID" id))))
            "\n")))))

(defun ndl-search-zotero--transform-creator (item &optional creator-indices)
  "Transform creator ITEM as JSON.
When given, CREATOR-INDICES holds creator index (著者標目) entries."
  (append
   (list :creatorType (pcase (map-elt item "区分")
                        ("著" "author")
                        ("編" "editor")
                        ("訳" "translator")
                        ("監修" "editor")
                        ("シリーズ著" "author")
                        ("シリーズ編" "seriesEditor")
                        (_
                         (message "Unknown role: %s" (map-elt item "区分"))
                         "contributor")))
   (let ((fullname (map-elt item "姓名")))
     (if-let* ((index (map-elt creator-indices fullname))
               (surname (car index))
               (given-name (cdr index)))
         (list :lastName surname :firstName given-name)
       (list :name fullname)))))

(defun ndl-search-zotero--transform-date (date)
  "Transform DATE as JSON.
If DATE is a list of form '(year month day)', it will be rendered as
'YYYY-MM-DD'."
  (if (listp date)
      (cond
       ((nth 2 date)
        (format "%04d-%02d-%02d" (nth 0 date) (nth 1 date) (nth 2 date)))
       ((nth 1 date)
        (format "%04d-%02d" (nth 0 date) (nth 1 date)))
       (t
        (format "%04d" (nth 0 date))))
    date))

(defun ndl-search-zotero--transform-language (lang)
  "Transform LANG as JSON."
  (pcase lang
    ("jpn" "ja")
    ("eng" "en")
    (_ lang)))

(provide 'ndl-search-zotero)
;;; ndl-search-zotero.el ends here
