Check if args are working:

reach run index hello world

Look for hello world as 2 separate args in the output

TODO:

reach run index hello 'Mr. Postman'

Should show 'Mr. Postman' as one arg, but it's being split on spaces.

workaround:

reach run index hello 'Mr.\ Postman'

