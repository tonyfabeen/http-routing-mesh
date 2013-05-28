require 'sinatra'

get '/test' do
 "<h3>You Tried Test</h3>"
end

get '/sleeping' do
  sleep 2000
  "<h3>After Sleep I will Wake UP</h3>"
end

