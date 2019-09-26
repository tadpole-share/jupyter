import docker
client=docker.from_env()
print(client.containers.run("ubuntu", "echo hello world"))
