FROM openjdk:11
RUN mkdir /app
COPY out/artifacts/eu24_admin_server_jar/ /app
WORKDIR /app
CMD java -jar eu24-admin-server.jar
