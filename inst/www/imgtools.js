
// change the src of an img without flicker
function set_image_src(src,id) {
  // create a new Image object
  var imgvar = new Image();
  // when preload is complete, apply the image to the img object
  imgvar.onload = function() {
    ('#'+id).src=src;
  };
  imgvar.src = src;
}
