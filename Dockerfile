# Use a Haskell base image with Stack installed
FROM haskell:latest

# Set working directory inside the container
WORKDIR /app

# Copy the rest of the project files to the container
COPY . /app

# Install GHC
RUN stack setup --install-ghc

# Build the Haskell project
RUN stack build

# Expose any necessary ports (if applicable)
EXPOSE 8080

# Command to run the Haskell project
CMD ["stack", "run"]
