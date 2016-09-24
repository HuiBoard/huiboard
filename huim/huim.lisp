;; Copyright (c) 2016 Joshua Miller

;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:huim)

(defvar *previous-word* nil)
(defvar *menu-level* 0)
(defvar *menu-buffer* nil)
(defvar *micro-buffer* nil)
(defvar *menu-hash* (make-hash-table :test 'equal))
(defvar *freq-file* #P"~/org/keyboard/freqs.txt")
(defvar *freq-hash*)
(defvar *null-vec* #*0000000000)
(defvar *alphabet* #("(null)" "(delimiter)" "(select)" "o" "(menu)" "c" "e" "t" "(negate)" "r" "w" "k" "n" "u" "y" "j" "i" "p" "s" "g" "b" "q" "x" "z" "m" "v" "d" "f" "l" "h" "a" "(infinity)"))
(defvar *dictionary*)

(defun search-string (pat string)
  (search pat string :test #'string=))

(defun remove-trailing-newline (string)
  "if the last character in a string is a \"#\Newline\", then remove the last character"
  (let ((len (length string)))
    (when (> len 1)
        (if (eq #\Newline (aref string (- len 1)))
         (subseq string 0 (- len 1))
         string))))

(defmacro shell-command (&rest strings)
  `(remove-trailing-newline (trivial-shell:shell-command (concatenate 'string ,@strings))))

(defun string-to-bitvector (string)
  "Takes a string of 1's and 0's and returns a bitvector"
  (let (ovec)
    (setf ovec (make-array (length string) :element-type 'bit))
    (dotimes (i (length ovec))
      (setf (aref ovec i) (parse-integer (subseq string i (1+ i)))))
    ovec))

(defun parse-bitvector-state (string)
  "Returns a bitvector from the Arduino line output."
  (string-to-bitvector (subseq string 1 11)))

(defun buffer-to-string (buffer)
  "Returns the micro-buffer contents as a string."
  (let ((ostring ""))
    (loop for x in (reverse buffer) do
          (setf ostring (concatenate 'string ostring x)))
    ostring))

(defmacro clear-buffer (buffer)
  `(setf ,buffer nil))

(defun send-n-backspaces (n)
  "Send n backspaces"
  (let ((ostring ""))
    (dotimes (i n)
      (setf ostring (concatenate 'string ostring " BackSpace")))
    (key ostring)))

(defun select-handler ()
  "If micro-buffer is empty, send a return keystroke. Otherwise, insert the word associated with the mnemonic proper subsequence currently in the micro-buffer."
      (if *micro-buffer*
        (progn
          (let (bword)
            (setf bword (best-word (buffer-to-string *micro-buffer*)))
            (when bword
              (send-n-backspaces (length *micro-buffer*))
              (send-string bword))
            (key "space") ;;insert space after word completion
            (when *micro-buffer*
              (setf *previous-word* (buffer-to-string *micro-buffer*)))
            (clear-buffer *micro-buffer*)
            (clear-menu)))
      (key "Return")))

(defun clear-menu ()
  "Sets menu level to zero, and clears menu-buffer"
  (setf *menu-level* 0)
  (clear-buffer *menu-buffer*))

(defun menu (key object &optional (application-name ""))
  "This function adds a menu shortcut to the menu hash table. Objects should be either strings that are sent as text, or a function name to be called."
  (when (not (hash-table-p (gethash application-name *menu-hash*)))
    (setf (gethash application-name *menu-hash*) (make-hash-table :test 'equal)))
  (setf (gethash key (gethash application-name *menu-hash*)) object))

(defun get-menu (key &optional (application-name ""))
  "Returns a menu shortcut object, which should be either a string or function name, from the menu hash."
  (when (hash-table-p (gethash application-name *menu-hash*))
    (gethash key (gethash application-name *menu-hash*))))

(defun menu-handler (key)
  "Handles menu items, checks for application specific menu items first."
  (let (menu-item)
    ;; first see if there is a menu entry for the current application
    (setf menu-item (get-menu key (current-application)))
    ;; otherwise get default item
    (when (not menu-item)
      (setf menu-item (get-menu key)))
    ;; if string or char, send keys; if function name, call function
    (cond ((not menu-item)
           (key "space"))
          ((stringp menu-item)
           (send-n-backspaces (length *menu-buffer*))
           (push menu-item *micro-buffer*)
           (send-string menu-item))
          ((characterp menu-item)
           (send-n-backspaces (length *menu-buffer*))
           (push (princ-to-string menu-item) *micro-buffer*)
           (send-char menu-item))
          ((functionp (symbol-function menu-item))
           (send-n-backspaces (length *menu-buffer*))
           (funcall menu-item))))
  (clear-menu))

(defun negate-handler ()
  ;; clear micro-buffer, then send backspaces
  (key "BackSpace")
  (if (> *menu-level* 0)
      (pop *menu-buffer*)
    (pop *micro-buffer*)))

(defun delimeter-handler ()
  "Sends the contents of the micro-buffer as keypresses, and saves it to *previous-word* so it can be added to a dictionary. Then inserts a space."
  (when *micro-buffer* (setf *previous-word* (buffer-to-string *micro-buffer*)))
  (key "space")
  (clear-buffer *micro-buffer*)
  (clear-menu)
  ;;optional, add . for double keypress? - if micro-buffer empty...

  )

(defun main-handler (string)
  (cond ((equal string "(menu)") (setf *menu-level* (1+ *menu-level*)))
        ((equal string "(null)") nil)
        ((equal string "(delimiter)") (delimeter-handler))
        ((equal string "(negate)") (negate-handler))
        ((equal string "(infinity)") (clear-menu))
        ((equal string "(select)") (select-handler))
        (t (if (> *menu-level* 0)
               (push string *menu-buffer*)
             (push string *micro-buffer*)) 
           (key string)
           (when (and (> *menu-level* 0) (eq *menu-level* (length *menu-buffer*)))
             (menu-handler (buffer-to-string *menu-buffer*))))))

(defun main-loop ()
  (serial:with-serial-device
   (device serial:serial-device-io
           :name *serial-port-path*
           :baudrate 115200
           :stopbits 1
           :parity #\N
           :canonp nil)
   (let ((stream (flexi-streams:make-flexi-stream device))
         (ostring "")
         (bvec nil)
         (lvec nil)
         (rvec nil)
         (svec *null-vec*)
         (lstring nil)
         (rstring nil))
     ;; read serial port
     (clear-buffer *micro-buffer*)
     (clear-menu)
     (loop
      (setf ostring (read-line stream))
      (setf bvec (parse-bitvector-state ostring))
      (if (equal bvec *null-vec*)
          (progn
            (setf lvec (subseq svec 0 5))
            (setf rvec (reverse (subseq svec 5 10)))
            (setf lstring (aref *alphabet* (bit-array-to-integer lvec)))
            (setf rstring (aref *alphabet* (bit-array-to-integer rvec)))
            ;; handle two handed chords
            (if (string-equal rstring "(menu)")
                (progn (main-handler rstring)
                       (main-handler lstring))
              (progn (main-handler lstring)
                       (main-handler rstring)))
            (setf svec *null-vec*))
        (setf svec (bit-ior svec bvec)))

      )
     )

   ))

(defun xdotool (string) (shell-command "xdotool " string))

(defun getwindowname () (xdotool "getwindowname"))

(defun getwindowfocus () (xdotool "getwindowfocus"))

(defun key (string) (xdotool (concatenate 'string " key --delay 2 " string)))

(defun send-keys (&rest strings)
  (loop for x in strings do
        (key x)))

(defun char-to-xcode-string (char)
  (concatenate 'string "0x" (write-to-string (char-code char) :base 16)))

(defun send-string (string)
  (let (ostring)
    (dotimes (i (length string))
      (setf ostring (concatenate 'string ostring " " (char-to-xcode-string (aref string i)))))
    (key ostring)))

(defun send-char (char)
  (key (char-to-xcode-string char)))

(defun current-application ()
  (let (app)
    (setf app (xdotool "getwindowname $(xdotool getwindowfocus)"))
    (cond ((search-string "- Chromium" app)
           "chromium")
          ((search-string "emacs@" app)
           "emacs")
          ((search-string "Guake!" app)
           "guake"))))

(defun set-capital-letters ()
  "sets the double chords to capital letters in the menu hash"
  (let (tempstring)
    (dotimes (i (length *alphabet*))
      (setf tempstring (aref *alphabet* i))
      (when (not (search-string "(" tempstring))
        (menu (concatenate 'string tempstring tempstring)
              (string-upcase tempstring))))))

(defun search-chars (string)
  "search codes for character names"
  (dotimes (i 1114111)
    (when (search-string (string-upcase string) (prin1-to-string (code-char i)))
      (prin1 (code-char i))
      (princ #\Newline))))

(defun fill-freq-hash ()
  ""
  (let (freqs split)
    (setf freqs (slurp-file *freq-file*))    
    (setf *freq-hash* (make-hash-table :test 'equalp))
    (setf freqs (cl-user:split-newlines freqs))
    (loop for x in freqs do
          (when x
            (setf split (cl-user:split-spaces x))
            (setf (gethash (first split) *freq-hash*)
                  (parse-integer (second split)))))))

(defun freq> (word1 word2)
  "compares the frequencies of two words"
  (let (num1 num2 val)
    (setf val nil)
    (setf num1 (gethash word1 *freq-hash*))
    (setf num2 (gethash word2 *freq-hash*))
    (if (and (integerp num1) (integerp num2))
        (when (> num1 num2) (setf val t))
      (when (and (integerp num1) (not (integerp num2)))
        (setf val t)))
    val))

(defun integer-to-bit-array (int length)
  "turns an integer into a bit array for use in making bit blades"
  (let (element raw stop)
    (setf stop nil)
    (setf raw (reverse (format raw "~B" int)))
    (setf element (make-array length :element-type 'bit))
    (dotimes (x (length raw))
      (if (string= (subseq raw x (+ x 1)) "1")
          (setf (bit element (- length (+ x 1))) 1)
        (setf (bit element (- length (+ x 1))) 0)))
    element))

(defun bit-array-to-integer (bit-array)
  (reduce #'(lambda (a b) (+ (ash a 1) b)) bit-array))

(defun print-alphabet (alphabet)
  "Prints the alphabet "
  (dotimes (i (length alphabet))
    (princ (integer-to-bit-array i 5))
    (princ " ")
    (princ (aref alphabet i))
    (princ #\Newline)))

(defun make-nword-array (word-list)
  "sorts words into an array holding lists of words of length n in slot n-1"
  (let (narray word)
    (setf narray (make-array (length (longest-string word-list))))
    (dotimes (i (- (length narray) 1))
      (setf (aref narray i) '()))
    (dotimes (i (- (length word-list) 1))
      (setf word (nth i word-list))
      (push word (aref narray (- (length word) 1))))
    narray))

(defun print-word-counts (word-list)
  "prints the number of words of length n in a list of words"  
  (let (nword-array)
    (setf nword-array (make-nword-array word-list))
    (dotimes (i (length nword-array)) 
      (princ i)
      (princ " ")
      (princ (length (aref nword-array i)))
      (princ #\Newline)
      )))

(defun string-to-character-list (string)
  "turns a string into a list of chars"
  (let (olist)
    (setf string (reverse string))
    (dotimes (i (length string))
      (push (aref string i) olist)
      )
    olist))

(defun longest-common-subsequence (string1 string2)
  "find the longest common subsequence of two strings, returns a list of chars"
  (lcs-list (string-to-character-list string1) 
            (string-to-character-list string2)))

(defun symmetric-chord-p (chord)
  "test if a chord is symmetric"
  (equal chord (reverse chord)))

(defun first-longest-string (list-of-strings)
  "searches for the length of the longest string(s) in list-of-strings, and returns the first string in the list with that length"
  (let (longest)
    (dotimes (i (length list-of-strings))
      (if (> (length (nth i list-of-strings)) (length longest))
          (setf longest (nth i list-of-strings))))
    longest))

(defun last-longest-string (list-of-strings)
  "searches for the length of the longest string(s) in list-of-strings, and returns the last string in the list with that length"
  (let (longest)
    (dotimes (i (length list-of-strings))
      (if (>= (length (nth i list-of-strings)) (length longest))
          (setf longest (nth i list-of-strings))))
    longest))

(defun factorial (n &optional (acc 1))
  (if (<= n 1)
      acc
    (factorial (- n 1) (* acc n))))

(defun n-choose-r (n r)
  (/ (factorial n) (* (factorial r) (factorial (- n r)))))

(defun print-algebra-counts (n)
  (let (summed step)
    (setf summed 0)
    (setf step 0)
    (dotimes (i (+ 1 n))
      (princ i) (princ " ")
      (setf step (n-choose-r n i))
      (princ step)(princ " ")
      (setf summed (+ summed step))
      (princ summed) (princ #\Newline))))

(defun dbg (&rest things-to-print) ;;add an optional stream
  (loop for x in things-to-print do
        (princ (princ-to-string x))
        (princ " "))
  (princ #\Newline)
  t)

(defun subseq-p (small large)
  "tests if the sequence small is a subsequence of large"
  (let (lpos spos counter pos)
    (setf lpos 0)
    (setf counter 0)
    (dotimes (i (length small))
      (setf spos i)
      (setf pos (search-string (subseq small i (+ i 1))
                            (subseq large lpos (length large))))
      (when pos
        (setf counter (+ 1 counter))
        (setf lpos (+ lpos pos))))
    (eq (length small) counter)))

(defun mnemonic-subseq-p (small large)
  "tests to see if the small sequence is a subsequence of large, and that first elemets match"
  (when (eq (aref small 0) (aref large 0))
    (subseq-p small large)))

(defun get-mseq (string)
  (let (olist) (loop for x in *dictionary* do
                     (when x
                       (when (mnemonic-subseq-p string x)
                         (push x olist))))
       (reverse olist)))

(defun length< (string1 string2)
  (< (length string1) (length string2)))

(defun sort-word-list-by-length (word-list)
  (sort word-list #'length<))

(defun sort-word-list-by-freq (word-list)
  (sort word-list #'freq>))

(defun print-best-words (seq)
  (let (words len)
    (setf words (sort-word-list-by-length (sort-word-list-by-freq (get-mseq seq))))
    (if (< (length words) 20)
        (setf len (length words))
      (setf len 20))

    (dotimes (i len)
      (print (nth i words)))
    (dbg "-- total words" (length words))))

(defun word-in-word-list-p (word word-list)
  "test to see if a word is in a word list"
  (loop for x in word-list do
        (when (string= x word)
          (return-from word-in-word-list-p t)))
  nil)

(defun best-word (seq)
  (let (mseqs oseq)
    (setf mseqs (get-mseq seq))
    (if (word-in-word-list-p seq mseqs)
        (setf oseq seq)
      (setf oseq (first (sort-word-list-by-length (sort-word-list-by-freq mseqs)))))
    oseq))

(defun blade-dimensionality (bit-array)
  "not needed?"
  (let (sum)
    (setf sum 0)
    (dotimes (i (length bit-array))
      (when (eq (aref bit-array i) 1) (setf sum (+ sum 1)))
      )
    sum))

(defun get-basis-elements (board-state-array)
  (let (lh rh olist)
    (setf lh (subseq board-state-array 0  5))
    (setf rh (reverse (subseq board-state-array 5 10)))
    (setf lh (aref *alphabet* (bit-array-to-integer lh)))
    (setf rh (aref *alphabet* (bit-array-to-integer rh)))
    (push rh olist)
    (push lh olist)
    olist))

(defun dictionary-file-to-word-list (file)
    "Returns a list of strings (words) from a dictionary file. The dictionary file must be constructed so there is one word per line."
    (let (dict)
      (setf dict (slurp-file file))
      (setf dict (split-sequence:split-sequence #\Newline dict))
      (setf (nth (- (length dict) 1) dict) nil)
      dict))

(defun slurp-stream (stream)
  (let ((seq (make-string (file-length stream))))
    (read-sequence seq stream)
    seq))

(defun slurp-file (file) (with-open-file (stream file) (slurp-stream stream)))

(defun huim-setup ()
  (setf *dictionary* (dictionary-file-to-word-list *dictionary-file-path*))    
  (fill-freq-hash)
  (set-capital-letters)

  )

(huim-setup)
