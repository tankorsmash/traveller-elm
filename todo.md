# Installs

* elm install lattyware/elm-fontawesome

# To Do

## MVP
* [X] Correct display of hex numbers across sectors
* [X] Ensure call to get star sectors includes all hexes on the map
* [X] Resolve issue with "Loading..." being displayed and never removed when scrolling
* [X] Account for offsets when dragging
* [X] Display sector coordinates in sidebar
* [X] click to drag map
* [X] use stars instead of solarsystems
** [X] fetch solar system when clicked
* [X] load list of sectors
** [X] display sector name in sidebar
* [X] fix clipping of map to viewport boundaries
* [X] support multi-sector viewing
* [X] making nicer function for turning a HexId into a row,col pair
* [X] save location of map
* [X] Only show known stellar objects in side panel
* [X] display Revelation location
* [X] display Revelation route
* [X] Display location in sector co-ordinates, not absolute co-ordinates
* [X] Remember position between sessions
* [X] table'd sidebar (recursively render the columns, account for SVG's width, since its HTML and elm-ui isn't aware of that size)
** [ ] like https://thriggle.netlify.app/traveller/subsector#$seed=Hide90811&$density=Standard&$size=Subsector&$rules=T5
* [ ] Convert back to SVG viewbox and don't render relative to the upper left hex
** [ ] Viewbox set to size of display, not fixed number of hexes
* [ ] modify UWP display based on survey index
* [ ] uncharted space link displays the full map of uncharted space
* [ ] Render regions
** [ ] API endpoint for regions
* [ ] sector selecter - centre map on the sector selected
* [ ] display name if known
* [ ] 3 Hex sizes
* [ ] scroll performance
* [ ] making HexAddress fail on invalid addresses
* [ ] render neutron star
* [ ] prettier ui somehow
* [ ] sector borders
* [ ] subsector borders

## 1.0
* [ ] smart caching/requesting the api so we don't fetch too much duplicate data

## Stretch Goals
* [ ] plot player route
* [ ] make sure nginx is gzipping

### Stretch Stretch Goals

* [ ] referee map for sophonts
* [ ] 60fps somehow
* [ ] export map to svg
