# syntax=docker/dockerfile:1

FROM python:3.11.0a6-alpine3.15
EXPOSE 2345
COPY . /app
WORKDIR /app
RUN pip install -r requirements.txt
CMD [ "python" "app.py" ]
