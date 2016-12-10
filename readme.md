# Emacs Package: Manage Frames V 0.0.3
Manage frames provides functionality to programitcly move the emacs frame around your display. The current version is limited to managing a single frame, however, further development is occurring to manage multiple frames accross multiple displays. Also need to rename many of the window functions frame, eg, small-frame.

## Installing
From your shell download the source from github:

```bash
## Change to a subdirectory in your local emacs folder
cd ~/.emacs.d/lisp/
curl --location -O https://raw.github.com/alexjgriffith/hotlaunch/master/manage-frames.el

## You can also clone the repository to your `emacs` home directory
cd ~/.emacs.d/ # or wherever your home repo is
clone https://github.com/alexjgriffith/manage-frames.git
cp manage-frames/manage-frames.el lisp/

## Be careful if you are using the develop branch, the current package is still imature and going
## through stages of overhaul, which could result in the changes of function names you depend on
## or the altering of functionality
```

Within your emacs init file (~/.emacs or ~/.emacs.d/init.el) add the following lines

```elisp
;; Make sure that emacs can see `manage-frames`
(add-to-list `load-path "~/.emacs/lisp")
(require 'manage-frames)
```

## Utilization
```elisp
;; toggle-window-size allows you to quickly transition from a maximized display
;; to a half screen display. To make this function easily accessible map it to
;; a keybinding in your initialization file.
(global-set-key (kbd "M-1") #'toggle-frame-size)

;; Set the frame size to half the width of the current monitor and then shift it
;; to the centre of the screen.
(global-set-key (kbd "M-2") 'frame-make-half-centered)

;; The make-window-function macro makes it easy to make your own custom frame designs
(make-window-function my-small-window-size
    20
    20
    off-x ; offset x and y are used to shift the frame between monitors and 
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
        (move-window-rightmost)  ; Rather than deleting the right windows
        (delete-other-windows)   ; this function deletes the window on the
        (max-full-off)))         ; left

;; By composing your new my-window-size function you can now close all but
;; the rightmost window and snap the frame to the right side of your monitor
(defun my-right-window()
    (interactive)
    (my-window-size)
    (right-window))

;; Your new window layout can be toggled with the max-window using the toggle-window-size
;; function.
(global-set-key (kbd "M-3") (lambda() (interactive)(toggle-window-size #'frame-move-right)))
```

## Version changes
Changes to  `manage-frames`

1. Changed the name of interactive functions to verbs/actions
2. Fixed the bug where the window would be two large for the current screen
3. Fixed the bug where you could not toggle to max window if the `mangage-frames-other-buff` had been destroyed
4. Added new interactive function to set the value of `mangage-frames-other-buff`, by default it will set the value to nil
5. Shifted development to the develop branch of the repository for future releases.

## Improvements to be made
1. Add a bug tracker
2. Fix the functionality on windows for multiple screen support
3. Test out `manage-frames` on more than two screens
4. Incorporate capacity to switch emacs frame between buffer
5. Enable management of multiple frames across multiple monitors / display devices

