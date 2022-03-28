Run `make react` to try it out.

Here's a video of Jay going through it:
https://youtu.be/57UeKj_Qu3E

If I were to improve this and "make it real" here are some things I'd do:

1. Add some meta information, like the name of the event, a URL for it, plus a
   logo, and maybe a color scheme.

1. Expose the meta information in a `View`.

1. Make the main site immediately go to the "Launch" mode and after it is done,
   generate a QR code for `https://rsvp.reach.sh/?ctc=THE_CONTRACT_INFO`.

1. Make it so if you go to one of the `?ctc` addresses, then it will see if
   you're the creator and immediately show the check-in UI and if not, then it
   will show the RSVP UI.

1. In the check-in UI, make it so the default is to scan the address with a QR
   code.

1. Allow the RSVP'er to decide how much they want to stake, with a minimum
   specified by the creator.

1. Have the RSVP'er pay a small amount to the Creator (because the Creator
   needs to pay fees to actually do the Check-in) --- Right now on ALGO, that
   would be 0.002 ALGOs.

1. In the RSVP UI, show information about how many people have RSVP'd and how
   much they've staked.
