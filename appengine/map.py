from google.appengine.dist import use_library
import os
use_library('django', '0.96')
from google.appengine.ext import db
from google.appengine.ext import webapp
from google.appengine.ext.webapp.util import run_wsgi_app
from google.appengine.ext.webapp import template

import entities

class MapPage(webapp.RequestHandler):
    def get(self):
        template_values = {
            'points': entities.Point.all(),
        }
        path = os.path.join(os.path.dirname(__file__), 'map.html')
        self.response.out.write(template.render(path, template_values))

application = webapp.WSGIApplication([('/.*', MapPage)], debug=True)

def main():
    use_library('django', '0.96')
    run_wsgi_app(application)

if __name__ == "__main__":
    main()
