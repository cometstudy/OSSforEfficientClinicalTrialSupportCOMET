#' @section Copyright: 
#' Copyright © 2021 University of Kansas

import smtplib, sys
from email.mime.text import MIMEText

# define content
recipients = sys.argv[1]
sender = "user2@kumc.edu"
subject = "report reminder"
body = sys.argv[2]


# make up message
msg = MIMEText(body, "plain")
msg['Subject'] = "COMET Overlapping File Noted - Please Inspect"
msg['From'] = sender
msg['To'] = recipients


# sending
session = smtplib.SMTP('university_drive', 25)
send_it = session.sendmail(sender, recipients, msg.as_string())
session.close()
