const _  = require('underscore')
const fs = require('fs')

const jsonServer = require('json-server')
const server                = jsonServer.create()
const jsonServerMiddlewares = jsonServer.defaults()
const jsonServerRouter      = jsonServer.router('db.json')

const db = JSON.parse(fs.readFileSync('db.json'))

// Allow CORS with localhost
server.all('*', (req, res, next) => {
  res.header('Access-Control-Allow-Origin', '*');
  return next();
});

/*
  Add custom routes:
    - '/random-entries'
    - '/unauthorized'
    - '/not-found'
  before json-server router
*/
server.get('/random-entries', (req, res) => res.jsonp(_.sample(_.uniq(db.entries), 5)))
server.get('/unauthorized',   (req, res) => res.sendStatus(401))
server.get('/not-found',      (req, res) => res.sendStatus(404))

/*
  Add default json-server middlewares:
    - logger
    - static
    - CORS
    - no-cache
  and json-server router
*/
server.use(jsonServerMiddlewares)
server.use(jsonServerRouter)

server.listen(
  3000,
  () => {
    console.log()
    console.log('  🚀  Serving db.json on http://localhost:3000  🚀  ')
    console.log()
  })
