# helm-google

Emacs Helm Interface for quick Google searches

## Screenshot

![screen shot](https://raw.github.com/steckerhalter/helm-google/master/screenshot.png)

## Installation with an El-Get Recipe

```lisp
(:name helm-google
       :type git
       :url "https://github.com/steckerhalter/helm-google")
```

## Usage

Call it with:

    M-x helm-google

Or bind it to a key:

```lisp
(global-set-key (kbd "C-h C--") 'helm-google)
```

If a region is selected it will take that as default input and search Google immediately. Otherwise it will start to search after you have entered a term. Pressing `RET` on a result calls the `browse-url` function which should open the URL in your external browser.

If you want to keep the search open use `C-z` instead of `RET`.
