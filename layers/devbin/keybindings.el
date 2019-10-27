;; Labels the app as aria se it doesn't appear as "prefix" in the menu
(spacemacs/declare-prefix "a a" "Aria")

;; The remaining useful keybindings to using Leetcode
(spacemacs/set-leader-keys
  "a a s" 'aria2-mode
  "a a u" 'aria2-add-uris
  "a a d" 'aria2-downloads-list
  "a a p" 'aria2-pause
  "a a r" 'aria2-resume
  "a a Q" 'aria2-terminate)
