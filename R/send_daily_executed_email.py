#' @section Copyright: 
#' Copyright © 2021 University of Kansas

import smtplib, sys, os
from email.mime.multipart import MIMEMultipart
from email.mime.text import MIMEText
from email.mime.base import MIMEBase 

# define content
sender_email = "user2@kumc.edu"
receiver_email = sys.argv[1]
text_plain = sys.argv[2]

recipients = [receiver_email,'study_coordinator_2@kumc.edu','user6@kumc.edu']


# make up message
msg = MIMEMultipart()
msg['Subject'] = text_plain
msg['From'] = sender_email
msg['To'] = ", ".join(recipients)


# Create the plain-text and HTML version of your message
text = text_plain

# Turn these into plain/html MIMEText objects
part1 = MIMEText(text, "plain")

# Add HTML/plain-text parts to MIMEMultipart message
# The email client will try to render the last part first
msg.attach(part1)


server =  smtplib.SMTP('university_drive', 25)
server.sendmail(sender_email, recipients, msg.as_string())
print('Email sent!')
server.close()
