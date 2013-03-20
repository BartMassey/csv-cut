# csv-cut
Copyright &copy; 2013 Bart Massey

This little UNIX command-line utility selects rows and
columns from a CSV file. Invoke it as follows:

        csv-cut [options] [<csv-file>]
          [-c,--cols <column-spec>]  Expression describing columns to select.
          [-r,--rows <row-spec>]     Expression describing rows to select.
          [<csv-file>]               CSV file to process.
      
A *<row-spec>* or *<column-spec>* is a list of rows and
columns to be selected. The syntax is comma separated
ranges, where each range is either an individual rows or
field (indexed starting at 1), or a dash-separated row or
field range in the usual syntax. If no spec is given for
rows or columns, all are selected; only one copy of repeated
specs is given. Examples:

        csv-cut -r 2             # select row 2, all columns
        csv-cut -r 2 -c 2        # select just row 2 column 2
        csv-cut -r 2-4           # select rows 2-4, all columns
        csv-cut -r -3            # select rows 1-3, all columns
        csv-cut -r -3,5-         # select rows 1-3 and 5-last, all columns
        csv-cut -r 3,3           # select just row 3 just once, all columns

The output is always written to standard output as a CSV
file with CRLF line terminators as suggested by [RFC
4180](http://tools.ietf.org/html/rfc4180).

This program should have a manpage, but I was too lazy to
write one.

To build this program, you'll need to have installed a
recent `ghc` and the packages `ssv`, `parseargs` and
`parsec-3` from [Hackage](http://hackage.haskell.org).  Then
just say "make".

This program is licensed under the "MIT License".  Please
see the file COPYING in the source distribution of this
software for license terms.
