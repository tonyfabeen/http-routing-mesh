$ redis-cli rpush http:www.myhost.com 192.175.19.19:80
$ redis-cli rpush http:www.mycrazyhost.com 192.175.19.20:80
$ redis-cli rpush websocket:websockets.mycrazyhost.com 192.175.19.20:4568


-- Pool App (gen_server) 
  Creates a new Router Proxy Instance related to some Host Name.
  Talk directly with Redis, when a new App instance is Created, it will
  create a new proccess to that instance. 


-- Websockets 
  Each Proxy instance will be a Erlang Process responding to a port.
  Router starts a session between Client and a Hosted Application
  
  ????
  How to Send Message to Backends ? Erlang CLients? 

  Each App can be a chat room with a list of subscribers(clients)and all messages will be sent to them when its ready..