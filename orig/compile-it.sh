#!/bin/sh
# Load CommonLoops, compile and test COOL.

CL=${CL-'/lisp/bin/cl'}  # change this to point to your local
                         # Common Lisp
PCL=${PCL-'/net/hplfs2/users/kempf/public/pcl'}

echo "Compiling Portable CommonLoops"
$CL <<EOF
#+HP(compile-file "defsys.l")
#-HP(compile-file "defsys.lsp")
(load "defsys")
(pcl::compile-pcl)
(sys::exit)
EOF

echo "Done Compiling Portable CommonLoops"

