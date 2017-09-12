# -*- mapserver -*-

PROJECTION
   "init=epsg:4326"
END

MAP
  OUTPUTFORMAT
    NAME png24
    DRIVER "GD/PNG"
    MIMETYPE "image/png"
    IMAGEMODE RGBA
    EXTENSION "png"
  END

  #CONFIG "MS_ERRORFILE" "M_SHAPEPATH`/mapserv_errors.txt'"

  IMAGETYPE      PNG24
  EXTENT         3.8 20.6 42.7 51.3
  SIZE           400 300
  SHAPEPATH      M_SHAPEPATH

  IMAGECOLOR     200 200 200
  TRANSPARENT    ON

  WEB
    METADATA
      "wms_srs" "EPSG:4326 EPSG:900913"
      "wms_name" "rodney"
      "wms_server_version" "1.1.0"
      "wms_formatlist" "image/png,image/jpeg"
      "wms_format" "image/png"
    END
  END

include(`layers.m4')

  SYMBOL
    NAME "circle"
    TYPE ellipse
    FILLED true
    POINTS
      1 1
    END
  END

  LAYER
    NAME         "photos"
    DATA         "photos_wpt"
    TYPE         POINT

    CLASS
      NAME       "Photos"

      STYLE
        SYMBOL "circle"
        COLOR 0 0 0
        OUTLINECOLOR 255 255 255
        SIZE 8
      END
    END
  END
END
