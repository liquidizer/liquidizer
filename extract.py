import xml.dom.minidom
import sys
import os
import re
     
#wget -r --restrict-file-names=ascii -I/Bundesparteitag_2011.2/Antragsportal https://wiki.piratenpartei.de/Bundesparteitag_2011.2/Antragsportal

path = 'Antragsportal/'
listing = os.listdir(path)
state= 0;

doc= xml.dom.minidom.Document()
root= doc.createElement("bpt")
doc.appendChild(root)

for filename in listing:
    print "Antrag: " + filename
    antrag= doc.createElement("antrag")
    root.appendChild(antrag)

    infile= open(path+"/"+filename,'r')
    text = infile.readlines()
    for line in text:
        if re.match('^<h3>.*Antragsnummer',line):
            state= 1
        elif re.match('^<h3>.*Antragstitel',line):
            state= 2
        elif re.match('^<h3>.*Antragstyp',line):
            state= 3
        elif re.match('^<h3>.*Liquid Feedback',line):
            state= 4
        elif re.match('^<h3>.*Wiki-Antragsfabrik',line):
            state= 5
        elif re.match('^<h3>.*Piratenpad',line):
            state= 6
        elif re.match('^<h3>.*Antragsgruppe',line):
            state= 7
        elif re.match('^<h3>.*',line):
            state= 0
        else:
            r= re.match('^<p>(.*)',line)
            if r:
              if state == 1:
                  antrag.setAttribute("nummer",r.group(1))
              elif state == 2:
                  antrag.setAttribute("titel",r.group(1))
              elif state == 3:
                  antrag.setAttribute("typ",r.group(1))
              elif state == 4:
                  wiki= re.match('.*href="([^"]*)"',line)
                  if wiki:
                      antrag.setAttribute("lqfb",wiki.group(1))
              elif state == 5:
                  wiki= re.match('.*href="(/Bundes[^"]*)"',line)
                  if wiki:
                      antrag.setAttribute("wiki",wiki.group(1))
              elif state == 6:
                  wiki= re.match('.*href="([^"]*)"',line)
                  if wiki:
                      antrag.setAttribute("pad",wiki.group(1))
              elif state == 7:
                  antrag.setAttribute("gruppe",r.group(1))
              state= 0

print doc.toprettyxml()    
