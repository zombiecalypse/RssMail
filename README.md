# RssMail

RSS is a great format to post updates on your blog, channel, issue tracker or
what not. However I found it annoying to synchronize the status across devices,
find a good reader for it, etc. It came to me that all this is pretty
well-solved for emails and coming to think of it, why not fill your email inbox
with RSS? You get filters, sync, and a plentitude of good ways to read them for
free!

## Installation

I wouldn't trust myself with email credentials, so RssMail doesn't bother to try
to be a SMTP client. Instead it used the `sendmail` utility.

1. Install and set up a `sendmail` provider, for example
   [ssmtp](https://wiki.debian.org/sSMTP).
2. Install `cabal`: `sudo apt-get install cabal-install`
3. Download RssMail `git clone https://github.com/zombiecalypse/RssMail.git`
4. `cd RssMail`
5. `cabal install`

## Usage

It's currently not really user friendly, but that's on my to-do list (the
project is like 1.5 days old).

1. `mkdir ~/.RssMail`
2. Edit `~/.RssMail/config.json`:
   ```json
       {
          "feeds" : [
             "http://jeremykun.com/feed/",
             ...
               ],
             "to" : {
               "name" : "Your Name",
               "email" : "yourname@example.com"
             }
       }
   ```

3. Run `RssMail` and receive a flood of emails.
4. Add a cron job for `RssMail` as required. It will only send you updates.
