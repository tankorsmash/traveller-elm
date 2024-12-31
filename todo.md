# To Do

## MVP
* [X] Account for offsets when dragging
* [X] Display sector coordinates in sidebar
* [X] click to drag map
* [X] use stars instead of solarsystems
** [ ] fetch solar system when clicked
* [X] load list of sectors
** [X] display sector name in sidebar
* [ ] fix clipping of map to viewport boundaries
* [ ] table'd sidebar (recursively render the columns, account for SVG's width, since its HTML and elm-ui isn't aware of that size)
* [X] support multi-sector viewing
* [X] making nicer function for turning a HexId into a row,col pair
* [ ] making HexAddress fail on invalid addresses
* [ ] Remember position between sessions
* [ ] render neutron star
* [ ] prettier ui somehow

## 1.0
* [ ] smart caching/requesting the api so we don't fetch too much duplicate data

## Stretch Goals
* [ ] plot player route
* [ ] make sure nginx is gzipping

### Stretch Stretch Goals

* [ ] referee map for sophonts
* [ ] 60fps somehow
* [ ] export map to svg
