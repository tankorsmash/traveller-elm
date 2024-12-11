# To Do

## MVP
* [ ] Account for offsets when dragging, now that we (well Josh) broke it in
	  8d8ef8b71c06e141d1b5081b1c4155326c192b94 when we made HexId.fromInt fail.
	  It's not clear why it broke, but its definitely that commit. It looks
	  like the offset isn't being applied when the scrolling is happening.

	  Zoom out and you'll see that we're rendering 2214 at its hex, but the hex
	  should be towards the top left of the screen, instead of the bottom
	  right. Not sure where that was determined, or if it was just broken
	  before and we didnt realize? I dunno!

* [X] Display sector coordinates in sidebar
* [X] click to drag map
* [ ] fix clipping of map to viewport boundaries
* [ ] table'd sidebar
* [X] support multi-sector viewing
* [X] making nicer function for turning a HexId into a row,col pair
* [ ] making the HexId codec fail on invalid hexids
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
