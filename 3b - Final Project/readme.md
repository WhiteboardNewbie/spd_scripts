## Text Editor
This one felt personally relevant. The job was to make a simple text editor (based on the image functionalities in Racket). It felt like such a fundamental technology that we take for granted with a non-trivial implementation so it was especially fun to work through this. The implementation involved the use of a "post" string and a "pre" string to render the cursor at the end of post and the beginning of pre. Of course, the arrow-keys change post and pre.
```

;; This program defines a simple one-line text editor
;; where the world state consists of a string text and a number cursor-pos.
;; It supports inserting characters at the cursor, moving the cursor left and right,
;; and deleting the character to the left of the cursor with backspace.


(require 2htdp/image)
(require 2htdp/universe)

;; A simple editor

;; Constants
;; =========

(define WIDTH 300)
(define HEIGHT 20)
(define MTS (empty-scene WIDTH HEIGHT))
(define CTR-Y (/ HEIGHT 2))
(define CTR-X (/ WIDTH 2))
(define CURSOR (rectangle 2 14 "solid" "red"))

(define TEXT-SIZE 14)
(define TEXT-COLOUR "black")

;; Data Definitions
;; ================

(define-struct editor (pre post))
;; Editor is (make-editor String String)
;; interp. pre is the text before the cursor, post is the text after
(define E0 (make-editor "" ""))
(define E1 (make-editor "a" ""))
(define E2 (make-editor "" "b"))

#;
(define (fn-for-editor e)
  (... (editor-pre e)
       (editor-post e)))

;; Functions
;; =========

;; Editor -> Editor
;; start the world with (main (make-editor "" "")
(define (main e)
  (big-bang e                     ; Editor
    (to-draw   render-editor)   ; Editor -> Image
    (on-key    handle-key)))      ; Editor KeyEvent -> Editor

;; Editor -> Image
;; render the current editor state
(check-expect (render-editor E0) (place-image (beside (text (editor-pre E0) TEXT-SIZE TEXT-COLOUR)
                                                      CURSOR
                                                      (text (editor-post E0) TEXT-SIZE TEXT-COLOUR))
                                              CTR-X
                                              CTR-Y
                                              MTS))
(check-expect (render-editor E1) (place-image (beside (text (editor-pre E1) TEXT-SIZE TEXT-COLOUR)
                                                      CURSOR
                                                      (text (editor-post E1) TEXT-SIZE TEXT-COLOUR))
                                              CTR-X
                                              CTR-Y
                                              MTS))
(check-expect (render-editor E2) (place-image (beside (text (editor-pre E2) TEXT-SIZE TEXT-COLOUR)
                                                      CURSOR
                                                      (text (editor-post E2) TEXT-SIZE TEXT-COLOUR))
                                              CTR-X
                                              CTR-Y
                                              MTS))

;; Template taken from Editor
(define (render-editor e) (place-image (beside (text (editor-pre e) TEXT-SIZE TEXT-COLOUR)
                                               CURSOR
                                               (text (editor-post e) TEXT-SIZE TEXT-COLOUR))
                                       CTR-X
                                       CTR-Y
                                       MTS))

;; Editor KeyEvent -> Editor
;; - when you type, characters should be inserted on the left side of the cursor 
;; - when you press the left and right arrow keys, the cursor should move accordingly  
;; - when you press backspace (or delete on a mac), the last character on the left of 
;;   the cursors should be deleted

(check-expect (handle-key E0 "left") E0)
(check-expect (handle-key E0 "right") E0)
(check-expect (handle-key E0 "\b") E0)
(check-expect (handle-key E1 "left") (make-editor "" "a"))
(check-expect (handle-key E1 "right") E1)
(check-expect (handle-key E1 "\b") E0)
(check-expect (handle-key E2 "left") E2)
(check-expect (handle-key E2 "right") (make-editor "b" ""))
(check-expect (handle-key E2 "\b") E2)


(define (handle-key e ke)
  (cond [(or (key=? ke "left") (key=? ke "right")) (shift-chars e ke)]
        [(key=? ke "\b") (remove-char e)]
        [else (add-char e ke)]))

;; Editor Keyevent -> Editor
;; Helper for handle-key, handles the logic for left/right arrow-keys
(define (shift-chars e ke)
  (cond [(key=? ke "left") (if (string=? (editor-pre e) "")
                               e
                               (make-editor (substring (editor-pre e)
                                                       0
                                                       (- (string-length (editor-pre e)) 1))
                                            (string-append (substring (editor-pre e)
                                                                      (- (string-length (editor-pre e)) 1)
                                                                      (string-length (editor-pre e)))
                                                           (editor-post e))))]
        [(key=? ke "right") (if (string=? (editor-post e) "")
                                e
                                (make-editor (string-append (editor-pre e)
                                                            (substring (editor-post e)
                                                                        0
                                                                        1))
                                             (substring (editor-post e)
                                                        1
                                                        (string-length (editor-post e)))))]))

;; Editor-> Editor
;; Helper for handle-key, removes the character left of the cursor for when ke="\b", if there is any
(define (remove-char e)
  (if (string=? (editor-pre e) "")
      e
      (make-editor (substring (editor-pre e)
                              0
                              (- (string-length (editor-pre e)) 1))
                   (editor-post e))))

;; Editor KeyEvent-> Editor
;; Helper for handle-key, adds a character left of the cursor for when a character (as a KeyEvent) is typed
(define (add-char e ke)
  (make-editor (string-append (editor-pre e)
                              ke)
               (editor-post e)))
```
![The text editor.](/assets/gifs/text-editor.gif)