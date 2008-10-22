dnl -*- mapserver -*-
  LAYER
    NAME         "M_DATE"
    DATA         "M_DATE`_trk'"
    TYPE         LINE
    METADATA
      "wms_srs" "EPSG:4326 EPSG:900913"
      "wms_name" "rodney"
      "wms_server_version" "1.1.0"
      "wms_formatlist" "image/png,image/jpeg"
      "wms_format" "image/png"
    END

    CLASS
      NAME       "M_NAME"

      STYLE
        OUTLINECOLOR M_COLOUR
      END
    END
  END

