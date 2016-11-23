# Emacs Package: Manage Frames V 0.0.1
Manage frames provides functionality to programitcly move the emacs frame arround your display. The current version is limited to managing a single frame, however, further development is occuring to manage multiple frames accross multiple displays. Also need to rename many of the window functions frame, eg, small-frame.

## Installing
From your shell download the source from github:

```bash
## Change to a subdirectory in your local emacs folder
cd ~/.emacs.d/lisp/
curl --location -O https://raw.github.com/alexjgriffith/hotlaunch/master/manage-frames.el
```

Within your emacs init file (~/.emacs or ~/.emacs.d/init.el) add the following lines

```elisp
;; Make sure that emacs can see hotlaunch
(add-to-list `load-path "~/.emacs/lisp")
(require 'manage-frames.el)
```

## Utilization
```elisp
;; toggle-window-size allows you to quickly transition from a maximized display
;; to a half screen display. To make this function easily accessable map it to
;; a keybinding in your initialization file.
(global-set-key (kbd "M-1") #'toggle-window-size)

;; The make-window-function macro makes it easy to make your own custom frame designs
(make-window-function my-small-window-size
    20
    20
    off-x ; offset x and y are used to shift the frame between montitors and 
    off-y ; within them. off-x and off-y are the start of your active monitor
    t)


;; Now for a more complex example
(make-window-function my-window-size
    (/ width 2) ; Half the width of the monitor
    height
    off-x 
    off-y
    t
    (lambda () 
        (rightmost)              ; Rather than deleting the right windows
        (delete-other-windows)   ; this function deletes the window on the
        (max-full-off)))         ; left

;; By composing your new my-window-size function you can now close all but
;; the rightmost window and snap the frame to the right side of your monitor
(function my-right-window()
    (interactive)
    (my-window-size)
    (right-window))
    
;; Your new window layout can be toggled with the max-window using the toggle-window-size
;; function.
(global-set-key (kbd "M-2") (lambda() (toggle-window-size my-right-window())))

```
