wget -r \
-I /Bundesparteitag_2011.1/Antragsportal,/Antragsportal/Anträge_BPT2011.1 \
--restrict-file-names=ascii \
http://wiki.piratenpartei.de/Bundesparteitag_2011.1/Antragsportal


#ls S*A* | sed -e p -e p | xargs -n 2 echo | cut -b 1-11,17- | xargs -n 2 mv
