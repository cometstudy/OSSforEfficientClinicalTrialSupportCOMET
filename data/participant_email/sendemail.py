
#Modified python script to send emails to comet participants
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

sender_email = "coordinator_email@kumc.edu"
receiver_email = sys.argv[1]

body = sys.argv[2]
body2 = open(body, 'r')
source_code = body2.read() 

prescription_name = sys.argv[3]
prescription = open(prescription_name, 'rb')
payload = MIMEBase('application', 'octate-stream', Name='Exercise_Prescription.pdf')
payload.set_payload((prescription).read())

fitbit_name = sys.argv[4]
fitbit_doc = open(fitbit_name, 'rb')
payload2 = MIMEBase('application', 'octate-stream', Name='Using_My_Fitbit.pdf')
payload2.set_payload((fitbit_doc).read())

trainer = sys.argv[5]

recipients = [receiver_email, 'study_coordinator_2@kumc.edu', trainer]
#recipients = [receiver_email, trainer]

msg2email = MIMEMultipart('related')
msg2email["Subject"] = "COMET Weekly Email"
msg2email["From"] = email.utils.formataddr(('Study Coordinator', sender_email))
msg2email["To"] = ", ".join(recipients)
msg2email.preamble = 'This is a multi-part msg2email in MIME format.'


msgAlternative = MIMEMultipart("alternative")
msg2email.attach(msgAlternative)

######### create the plain MIMEText object #############
text = """\
If you see this please contact study_coordinator_2@kumc.edu and let the him know you cannot see
the html version of this email.
"""
msgText = MIMEText(text, 'plain', 'utf-8')
msgAlternative.attach(msgText)


######## Create the html  version of your msg2email ############

html=source_code
         
msgHTML = MIMEText(html, "html", 'utf-8')
msgAlternative.attach(msgHTML)

######## Attach 2 pdfs ###########

# enconding the binary into base64
encoders.encode_base64(payload)
 
# add header with pdf name
payload.add_header('Content-Decomposition', 'attachment', filename='Exercise_Prescription.pdf')
msg2email.attach(payload)

# enconding the binary into base64
encoders.encode_base64(payload2)
 
# add header with pdf name
payload2.add_header('Content-Decomposition', 'attachment', filename='Using_My_Fitbit.pdf')
msg2email.attach(payload2)
####### Create secure connection with server and send email ########

try:
   server =  smtplib.SMTP('university_drive', 25)
   #server.set_debuglevel(0)
   #server.ehlo()
   #server.starttls()
   server.sendmail(sender_email, recipients, msg2email.as_string())
   print('successfully sent the mail')
   server.close()
except:
        print("failed to send mail")
#if __name__ == '__main__':
#  send_email()





