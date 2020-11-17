#!/usr/bin/python

# Largely copied from pygments-custom-cpplexer
# https://github.com/FSund/pygments-custom-cpplexer/blob/master/setup.py

from setuptools import setup, find_packages

setup(
    name='pygments-reach',
    description='Pygments lexer for Reach.',
    long_description=open('README.md').read(),
    keywords='pygments reach lexer',

    packages=find_packages(),
    install_requires=['pygments >= 1.4'],
    # Temporarily use the master branch of pygments--which highlights
    # `await`/`async` in JS files-- until the 2.7.3 release.
    dependency_links=['https://github.com/pygments/pygments/tarball/master#egg=pygments-2.7.3'],

    entry_points='''[pygments.lexers]
                    reach=pygments_reach:ReachLexer''',

    classifiers=[
        'Development Status :: 4 - Beta',
        'Environment :: Plugins',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Programming Language :: Python :: 2',
        'Programming Language :: Python :: 3',
        'Topic :: Software Development :: Libraries :: Python Modules',
    ],
)
