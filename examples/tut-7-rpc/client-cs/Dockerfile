FROM mcr.microsoft.com/dotnet/sdk:6.0-alpine AS build
WORKDIR /app
COPY    reachrpc.cs /app/
COPY    . /app/cs/src/rpc/

CMD     ["dotnet", "run", "--framework net471"]


