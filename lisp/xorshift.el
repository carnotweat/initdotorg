;;; xorshift.el --- xorshift implementation in Emacs Lisp -*- lexical-binding: t; -*-

;; Copyright (C) 2015 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-xorshift
;; Version: 0.01

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See https://en.wikipedia.org/wiki/Xorshift

;;; Code:

(require 'cl-lib)

(defmacro xorshift--set-seed-uint32 (seed)
  `(unless ,seed
     (setq ,seed (random (1- (expt 2 32))))))

;;;###autoload
(defun xorshift-builder (&optional seed)
  (xorshift--set-seed-uint32 seed)

  (let ((y seed))
    (lambda ()
      (setq y (logxor (lsh y 13)))
      (setq y (logxor y (lsh y -17)))
      (setq y (logxor y (lsh y 5)))
      y)))

;;;###autoload
(defun xorshift96-builder (&optional seed1 seed2 seed3)
  (xorshift--set-seed-uint32 seed1)
  (xorshift--set-seed-uint32 seed2)
  (xorshift--set-seed-uint32 seed3)

  (let ((x seed1) (y seed2) (z seed3))
    (lambda ()
      (let ((tmp (logxor (logxor x (lsh x 3))
                         (logxor y (lsh y -19))
                         (logxor z (lsh z 6)))))
        (setq x y)
        (setq y z)
        (setq z tmp)
        z))))

;;;###autoload
(defun xorshift128-builder (&optional seed1 seed2 seed3 seed4)
  (xorshift--set-seed-uint32 seed1)
  (xorshift--set-seed-uint32 seed2)
  (xorshift--set-seed-uint32 seed3)
  (xorshift--set-seed-uint32 seed4)

  (let ((x seed1) (y seed2) (z seed3) (w seed4))
    (lambda ()
      (let ((tmp (logxor x (lsh x 11))))
        (setq x y)
        (setq y z)
        (setq z w)
        (setq w (logxor (logxor w (lsh w -19)) (logxor tmp (lsh tmp -8))))
        w))))

(provide 'xorshift)

;;; xorshift.el ends here
