# -*- mapserver -*-

MAP
  OUTPUTFORMAT
    NAME png24
    DRIVER "GD/PNG"
    MIMETYPE "image/png"
    IMAGEMODE RGBA
    EXTENSION "png"
  END

  CONFIG "MS_ERRORFILE" "M_SHAPEPATH`/mapserv_errors.txt'"

  IMAGETYPE      PNG24
  EXTENT         3.8 20.6 42.7 51.3
  SIZE           400 300
  SHAPEPATH      M_SHAPEPATH

  IMAGECOLOR     200 200 200
  TRANSPARENT    ON

#   http://crschmidt.net/blog/311/using-tilecache-with-google-maps-and-virtual-earth/
#  PROJECTION
#    "init=epsg:900913"
#  END


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
        SIZE 2
      END
    END
  END
END
