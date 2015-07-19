# Implement a mail delivery method

Every Cms should be capable of sending emails. LambdaCms is no exception. Sending mails should also be flexible. Some might want to use `sendmail`, others might want to send it over `SMTP`. For this reason a function to actually send an email should be implemented in the base app. LambdaCms's default is to print it to stdout.

Constructing a Mail is done using the `Network.Mail.Mime` package. This packages gives us the datatype `Mail` to represent an entire mail message.

It is defined as:

```haskell
data Mail = Mail
    { mailFrom :: Address
    , mailTo   :: [Address]
    , mailCc   :: [Address]
    , mailBcc  :: [Address]
    , mailHeaders :: Headers
    , mailParts :: [Alternatives]
    }
    deriving Showa
```

From, To, Cc and Bcc are self explanatory. Headers is for *other* headers (that are not from, to, cc or bcc). And Parts is for the Text and Html version of the message. Attachments go in here as well.

Other packages can handle the `Mail` type or a ` ByteString` which is what you get by calling `renderMail'`.

More on Mail.Mime can be found [here](http://hackage.haskell.org/package/mime-mail-0.4.6/docs/Network-Mail-Mime.html).

## How is it Defined

The LambdaCmsAdmin class has a function called `lambdaCmsSendMail`.

```haskell
lambdaCmsSendMail :: master -> Mail -> IO ()
```

It simply takes a `Mail` and returns an `IO ()`

The default implementation looks like:

```haskell
lambdaCmsSendMail _ (Mail from tos ccs bccs headers parts) = putStrLn -- ...
```

## Sending with sendmail

Using `sendmail` as delivery method is the easiest way. This function is included in the `Network.Mail.Mime` package. All it takes is a rendered message:

```haskell
renderMail' :: Mail -> IO ByteString
sendmail :: ByteString -> IO ()
```

or just do it in one go:

```haskell
renderSendMail :: Mail -> IO ()
```

Implementing this in a base app is as simple as:

```haskell
lambdaCmsSendMail = lift . renderSendMail
```


## Sending with SMTP

We've used [HaskellNet](http://hackage.haskell.org/package/HaskellNet) and [HaskellNet-SSL](http://hackage.haskell.org/package/HaskellNet-SSL) for sending mails over SMTP. These aren't included in the stackage we use so we cloned them from github. This also requires [base64-string](http://hackage.haskell.org/package/base64-string) to be installed (also not on our current stackage). How to install extra packages is described [here](https://github.com/lambdacms/lambdacms-core#other-packages).


HaskellNet comes with it's own `sendMail` which has the signature:

```haskell
sendMail :: String     -- ^ sender mail
         -> [String]   -- ^ receivers
         -> ByteString -- ^ data
         -> SMTPConnection
         -> IO ()
```

This doesn't look like what we got from the Mail type at all, so we have to create a few extra functions to make it work. We need to grab the sender's email address from the Address type and do the same for the receivers.

```haskell

senderMailAddress :: Mail -> String
senderMailAddress = T.unpack . addressEmail . mailFrom

receiverMailAddresses :: Mail -> [String]
receiverMailAddresses (Mail _ tos ccs bccs _ _) = map (T.unpack) (tos' ++ ccs' ++ bccs')
    where tos' = maddress tos
          ccs' = maddress ccs
          bccs' = maddress bccs
          maddress = map address
          address (Address n e) = e
```

Then we have to render the mail to get a ByteString for the data part.

```haskell
renderedEmail <- renderMail' mail >>= return . LB.toStrict
```

and lastly tie it all together to actually send it:

```haskell
import qualified Network.HaskellNet.SMTP as S
import Network.HaskellNet.SMTP.SSL
import Network.HaskellNet.Auth (AuthType(LOGIN))
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text as T

lambdaCmsSendMail _ mail = do
  renderedEmail <- renderMail' mail >>= return . LB.toStrict
  smtp <- connectSMTPSTARTTLS smtpServer
  _ <- S.sendCommand smtp $ S.AUTH LOGIN username password
  S.sendMail (senderMailAddress mail) (receiverMailAddresses mail) renderedEmail smtp
  S.closeSMTP smtp
  where username = "" -- smtp username
        password = "" -- smtp password
        smtpServer = "" -- smtp server, e/g smtp.gmail.com
        senderMailAddress :: Mail -> String
        senderMailAddress = T.unpack . addressEmail . mailFrom
        receiverMailAddresses :: Mail -> [String]
        receiverMailAddresses (Mail _ tos ccs bccs _ _) = map (T.unpack) (tos' ++ ccs' ++ bccs')
          where tos' = maddress tos
                ccs' = maddress ccs
                bccs' = maddress bccs
                maddress = map address
                address (Address _ e) = e
```

# How to use

How to use it inside a handler:

```haskell
    y <- getYesod
    lcsm <- liftIO $ simpleMail
            (Address Nothing "mail@to.here")
            (Address (Just "LambdaCms") "mail@from.here")
            "This is a subject!"
            "plain body"
            "html body"
            []

    -- maybe add an attachment
    lcsm' <- liftIO $ addAttachment "a text" "/path/to/a.file" lcsm

    -- send it!
    liftIO $ lambdaCmsSendMail y lcsm'

```


## TODO

* Document handling exceptions
* Document using a service like sendgrid or mailgun
* Try the instruction above, and check if they still work
