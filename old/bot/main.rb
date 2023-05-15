#!/usr/bin/env ruby

require 'socket'

sock = TCPServer.open "localhost", 4321
client = sock.accept
loop do
	message = client.gets.chomp
	puts message
	client.puts "pong" 
end