#!/usr/bin/env ruby

require 'socket'

sock = TCPSocket.open "localhost", 4321
sock.puts "ping"
loop do
    response = sock.gets.chomp
    puts response
    sock.puts "ping"
end