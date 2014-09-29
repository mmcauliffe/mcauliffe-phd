
 (define (resize-bitmap infilename
                        outfilename
                              new-height)
   (let* ((image (car (gimp-file-load RUN-NONINTERACTIVE infilename infilename)))
          (drawable (car (gimp-image-get-active-layer image)))
          (cur-width  (car (gimp-image-width image)))
          (cur-height (car (gimp-image-height image)))
          (ratio      (min (/ new-width cur-width) (/ new-height cur-height)))
          )
     (plug-in-unsharp-mask RUN-NONINTERACTIVE
                       image drawable radius amount threshold)
     (gimp-file-save RUN-NONINTERACTIVE image drawable filename filename)
     (gimp-image-delete image)))
