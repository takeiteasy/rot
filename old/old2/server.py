#!/usr/bin/env python3
import asyncio
import random
import websockets
import json

async def recv(websocket):
    asyncio.create_task(send(websocket))
    while True:
        try:
            message = await websocket.recv()
            print(message)
        except websockets.ConnectionClosedOK:
            break

async def send(websocket):
    while True:
        data = []
        try:
            await websocket.send(json.dumps(data))
        except websockets.ConnectionClosedOK:
            break
        await asyncio.sleep(0.5)

async def main():
    async with websockets.serve(recv, "localhost", 8765):
        await asyncio.Future()

if __name__ == "__main__":
    asyncio.run(main())