#' @section Copyright: 
#' Copyright © 2021 University of Kansas

import smtplib, sys
from email.mime.text import MIMEText

# define content
recipients = sys.argv[1]
sender = "user2@kumc.edu"
body = sys.argv[2]


# make up message
msg = MIMEText(body, "plain")
msg['Subject'] = "Failed - COMET Daily Script"
msg['From'] = sender
msg['To'] = recipients


# sending
session = smtplib.SMTP('university_drive', 25)
send_it = session.sendmail(sender, recipients, msg.as_string())
session.close()
