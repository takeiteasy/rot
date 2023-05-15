#!/usr/bin/env ruby

require 'socket'
require 'timeout'
require 'json'

class IO
	def gets_nonblock
		@rlnb_buffer ||= ""
		ch = nil
		while ch = self.read_nonblock(1)
			@rlnb_buffer += ch
			if ch == "\n" then
				res  = @rlnb_buffer
				@rlnb_buffer = ""
				return res
			end
		end
	end
end

module Emitter
	def callbacks
		@callbacks ||= Hash.new { |h, k| h[k] = [] }
	end
	
	def on type, &block
		callbacks[type] << block
		self
	end
	
	def emit type, *args
		callbacks[type].each do |block|
			block.call *args
		end
	end
end

class Time
  def to_ms
	(self.to_f * 1000.0).to_i
  end
end

class TimeEvent
	def initialize to, rot, block
		@lastTime = Time.now.to_ms
		@elapsedTime = 0
		@timeout = to
		@removeOnTrigger = rot
		@callback = block
	end
	
	def step *args
		now = Time.now.to_ms
		@elapsedTime += now - @lastTime
		@lastTime = now
		if @elapsedTime > @timeout
			@elapsedTime = @elapsedTime - @timeout
			@callback.call *args
			return false if @removeOnTrigger
		end
		true
	end
end

module Timer
	def events
		@events ||= Array.new
	end
	
	def every n, &block
		events << TimeEvent.new(n, false, block)
		self
	end
	
	def after n, &block
		events << TimeEvent.new(n, true, block)
		self
	end
	
	def step *args
		@events = events.select do |event|
			event.step(*args)
		end
	end
end

module EventStream
	include Emitter
	include Timer
	attr_accessor :io, :buf
	
	def initialize serv, port
		@buf = []
		Timeout.timeout(5) { @io  = TCPSocket.new serv, port }
	rescue SocketError => e
		puts "ERROR: Failed to connect to #{serv}! #{e.message}"
		abort
	rescue Timeout::Error
		puts "ERROR: Connection to #{serv} timed out!"
		abort
	end
	
	def read
		read = @io.gets_nonblock
		emit :READ, read
	rescue IO::WaitReadable
		emit :WAITING
	rescue EOFError, Errno::ECONNRESET
		emit :CLOSED
	end
	
	def << data
		@buf << data
	end
	
	def flush
		@buf.each do |x|
			@io.puts x
			emit :WROTE, x
		end
		@buf = []
	rescue EOFError, Errno::ECONNRESET
		emit :CLOSED
	end

	def tick
		step
		read
		flush
		sleep 0.001
	end
end

class Stream
	include EventStream
end

if __FILE__ == $0
	bot = Stream.new "localhost", 4321
#	game = Stream.new "localhost", 1234
	
	while true
		bot << "test"
		bot.tick
#		game.tick
	end
end
