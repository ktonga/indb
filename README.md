INDB
====

__Work in progress__

INDB is a set of libraries, utilities and applications to help you decide what to watch next. It intends to achieve that by augmenting the titles on your Netflix list with extra information from IMDB, so that you can filter and sort the titles using the needed criteria to let you find exactly what you are looking for.

## Modules

 - [ ] **Core library**: exposes functions to get the merge of all sources. It will return the list of titles plus all the extra info from IMDB to be used by the apps
 - [ ] **Export to file**: saves the enhanced titles as a local file in multiple formats
 - [ ] **Export to IMDB**: adds the titles to a list in your IMDB account
 - [ ] **Command line**: app that lets you use all the functionality from the libs quickly from the terminal
 - [ ] **REST endpoint**: exposes all the functionality to be accessed over HTTP
 - [ ] **Web interface**: simple and small web app to work with your list on the browser

## TODO

- [ ] Externalize parameters. Command args? Config file?
- [ ] Support for VPN, Tag with Netflix region.
- [ ] Good error handling. Parse OMDB and IMDB response to check status and error message
- [ ] Interactive mode to recover skipped titles. Search wihout (...) and let user choose result
- [ ] Synchronize. Detect diffs between Netflix and IMDB and only update that

