HTTP Routing Mesh
=================

# What is ?

It's a simple HTTP Router and proxy based on Erlang. 

## Dependencies

 - >= Erlang R15B03
 - >= Cowboy 0.8.5
 - Redis

## Run it Locally

### Change /etc/hosts

Put line above on /etc/hosts 

```sh
127.0.0.1 www.myhost.com

```

### Populate Redis
  Here i populate **www.myhost.com** with 02 backend servers

```sh
  $ redis-cli RPUSH http:www.myhost.com 127.0.0.1:4567

  $ redis-cli RPUSH http:www.myhost.com 127.0.0.1:4568
```

### Run Sinatra test 

```sh
$ ./ruby app_test.rb -p 4567
```

```sh
$ ./ruby app_test.rb -p 4568 
```

### Compile and Start Router 

```sh
  $ make
```

It's ready.

By Default the router starts on port 8080, if you want change the number of socket acceptors and port listening, open **.app.src** file and change values there. 

To access the Sinatra application test try :

```sh
http://www.myhost.com:8080
```

## TODO

Everything.
