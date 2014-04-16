group-payments
==============

A script to simplify people paying each other for items in groups.

How to use
----------

Run as follows

    $ runhaskell payments.hs < input.txt

Input file format
-----------------

You need an input file where each line conforms to the following format (where the '`<`' and '`>`' aren't in your file; they're just delimiters for purposes of describing input):

    <item_name> <payer_name> <list_of_recipients_of_item> <cost>

Each segment (e.g. <payer name>) can have no spaces, and <cost> must be a number. Additionally, <list of recipients of item> must be of the following format:

    [<name_0>,<name_1>,...,<name_n>]

, where there are *no spaces* after the commas, and the '`[`' and '`]`' are present.

See `input.txt` for an example.
