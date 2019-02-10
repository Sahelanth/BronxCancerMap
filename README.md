README.md

Interactive map that has a little dashboard pop up on clicks. See http://rpubs.com/Sahelanth/bronxcancermap

Good way to present more detailed info than a simple popover would, without making the interface more complicated.
The trick is just generating the HTML for a couple different Leaflet popup objects, then editing the
HTML to put them together. 

Features to add: see the replies here https://twitter.com/Sahelanth/status/1051281234142355456

Also, want to generate the dashboard graphs dynamically, rather than save them. Current approach can't scale to a nationwide map with same level of detail.