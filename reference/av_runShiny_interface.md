# Helper functions for analytics

Displays a message underneath an input box

Copies a data.frame to the clipboard, with a status message if relevant

Sets the title for the Details tab

## Usage

``` r
quick_message(wh, this_message = "", eval = TRUE, color = "#1f78b4")

avsh_clipboard(x, title = "")

avsh_set_tabtitle(newtext = "DETAIL", tabnm = "detail", makefocus = TRUE)
```

## Arguments

- wh:

  inputID for shiny element to put a message underneath of. See
  Documentation and/or Code

- this_message:

  (default "") A text message to be used. If empty string, the current
  message is cleared.

- eval:

  (default TRUE) OPtional parameter to suppress execution.

- color:

  Optional text color

- x:

  A `data.frame` or equivalent.

- title:

  String to add to a message printed if relevant

- newtext:

  (default"DETAIL") What to name the tab as

- tabnm:

  (default "detail") inputID of relevant tab

- makefocus:

  (default: TRUE) Upon setting the tab title, select the tab.

## Value

logical value of `eval`

Nothing

Nothing
