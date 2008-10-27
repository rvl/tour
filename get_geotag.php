#!/usr/bin/php
<?php

$GEOTAG_CMD = "exiftool -t -S -q -f -n -Directory -FileName -GpsLatitude -GpsLongitude";

if ($argc < 2) {
  echo "Usage: $argv[0] YYYYMMDD\n";
  exit(0);
 }

$year = substr($argv[1], 0, 4);
$month = substr($argv[1], 4, 2);
$day = substr($argv[1], 6, 2);

$full_path = "tour$year/$month/$day";

require_once(dirname(__FILE__) . "/../gallery2/embed.php");
/* connect to embed library */
$ret = GalleryEmbed::init(array('fullInit' => true));

list ($error,$id) = GalleryCoreApi::fetchItemIdByPath($full_path);

if (!$error) {
  list ($error,$items) = GalleryCoreApi::loadEntitiesById(array($id));
  if(!$error) {
    list ($error, $ids) = GalleryCoreApi::fetchChildItemIds($items[0]);
    if (!$error) {
      list ($error, $subitems) = GalleryCoreApi::loadEntitiesById($ids);
      if (!$error) {
	foreach ($subitems as $item) {
	  list ($error, $path) = $item->fetchPath();
	  $id = $item->id;
	  echo "$id ";
	  system("$GEOTAG_CMD $path");
	}
      }
    }
  }
 }

?>
