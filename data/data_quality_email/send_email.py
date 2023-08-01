
#Python script to send emails to physicians
#Written on 2020-06-14 by EDV and JC

# Send an HTML email with an embedded image and a plain text messagel for
# email clients that don't want to display the HTML.
import smtplib, sys, email, os, urllib
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from email.mime.image import MIMEImage
from email.utils import formataddr
from email.mime.base import MIMEBase
from email import encoders
#from email.mime.base import MIMEBase

sender_email = "study_coordinator_2@kumc.edu"
receiver_email = sys.argv[1]
body = sys.argv[2]
body2 = open(body, 'r', encoding='utf-8')
source_code = body2.read() 

attachment = sys.argv[3]


  
recipients = [receiver_email,'study_coordinator_2@kumc.edu']
#recipients = [receiver_email]

msg2email = MIMEMultipart('related')
msg2email["Subject"] = "COMET Data Quality Email"
msg2email["From"] = email.utils.formataddr(('study_coordinator_2', sender_email))
msg2email["To"] = ", ".join(recipients)
msg2email.preamble = 'This is a multi-part msg2email in MIME format.'


msgAlternative = MIMEMultipart("alternative")
msg2email.attach(msgAlternative)

######### create the plain MIMEText object #############
text = """\
If you see this please contact study_coordinator_2@kumc.edu to let him know you cannot see
the html version of this email.
"""
msgText = MIMEText(text, 'plain', 'utf-8')
msgAlternative.attach(msgText)


######## Create the html  version of your msg2email ############

html=source_code
         
msgHTML = MIMEText(html, "html", 'utf-8')
msgAlternative.attach(msgHTML)

#########  Attach html of report #########

filename = "COMET Data Quality Email.html"
attachment = open(attachment, "rb")
  
part = MIMEBase('application', 'octet-stream')
part.set_payload((attachment).read())
encoders.encode_base64(part)
part.add_header('Content-Disposition', "attachment; filename= %s" % filename)
  #pdf.add_header('content-disposition', 'attachment', filename=basename(attachment_path))
#f=codecs.open(attachment, 'r')

msg2email.attach(part)



####### Create secure connection with server and send email ########

try:
   server =  smtplib.SMTP('university_drive', 25)
   server.sendmail(sender_email, recipients, msg2email.as_string())
   print('successfully sent the mail')
   server.close()
except:
        print("failed to send mail")
#if __name__ == '__main__':
#  send_email()





