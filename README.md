INDB
====

__Work in progress__

INDB is an utility program which lets you populate (synchronize) a list in your IMDB account with the titles in "My List" from your Netflix account.
It's particularly useful if you have a lot of titles in your list and you need to apply some filtering/sorting in order to find the next cool thing to watch. As you might already noticed, Netflix does not support any of those features in their application but IMDB does support them.

## TODO

- [ ] Externilize parameters. Commad args? Config file?
- [ ] Tag with Netflix region. Add Netflix country as title description
- [x] Better error handling. Parse OMDB and IMDB response to check status and error message
- [ ] Interactive mode to recover skipped titles. Search wihout (...) and let user choose result
- [ ] Synchronize. Detect diffs between Netflix and IMDB and only update that

