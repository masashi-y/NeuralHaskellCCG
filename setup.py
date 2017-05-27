#! -*- coding: utf-8 -*-

# from __future__ import unicode_literals, print_function
from distutils.core import setup
from distutils.extension import Extension
from Cython.Distutils import build_ext
import numpy
import distutils


compile_options = "-O3 -g -fpic -march=native"

ext_modules = [
        Extension("chainer_tagger",
                  ["py/tagger.pyx"],
                  include_dirs=[numpy.get_include()],
                  extra_compile_args=compile_options.split(" "),
                  ),
        ]

setup(
        name = "chainer_tagger",
        cmdclass = { "build_ext" : build_ext },
        ext_modules = ext_modules,
)
