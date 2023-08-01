
#Python script to send emails to physicians
#Written on 2020-06-14 by EDV and JC

# Send an HTML email with an embedded image and a plain text message for
# email clients that don't want to display the HTML.
import smtplib, sys
from io import open
from email.mime.text import MIMEText
#from email.mime.base import MIMEBase


# define content
recipients = sys.argv[1]
sender = sys.argv[3]
body = sys.argv[2]
body2 = open(body, 'r', encoding='utf-8')
text = body2.read() 



# make up message
msg = MIMEText(text, "plain")
msg['From'] = sender
msg['To'] = recipients



####### Create secure connection with server and send email ########

try:
   server =  smtplib.SMTP('university_drive', 25)
   server.sendmail(sender, recipients, msg.as_string())
   print('successfully sent the mail')
   server.close()
except:
        print("failed to send mail")






