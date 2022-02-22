ARG DOTNET_RUNTIME_IMAGE
ARG DOTNET_SDK_IMAGE
FROM $DOTNET_SDK_IMAGE AS build
WORKDIR /source

COPY *.csproj .
RUN dotnet restore
COPY . .
RUN dotnet publish -c release -o /app --no-restore

FROM $DOTNET_RUNTIME_IMAGE
WORKDIR /app
COPY --from=build /app .
ENTRYPOINT ["/app/index"]

