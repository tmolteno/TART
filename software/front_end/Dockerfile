# Rust build stage
FROM rustlang/rust:nightly-slim AS rust-build
WORKDIR /app/rust
RUN apt-get update && apt-get install -y curl
RUN curl https://rustwasm.github.io/wasm-pack/installer/init.sh -sSf | sh
COPY rust /app/rust
RUN RUST_LOG=info wasm-pack build --release --out-dir ./pkg

# Web App build stage
FROM node:lts-alpine as node-build-stage
COPY tart-vuer /app/tart-vuer
COPY --from=rust-build /app/rust/pkg /app/tart-vuer/pkg
WORKDIR /app/tart-vuer
RUN yarn

ARG CI_PROJECT_NAME
ENV CI_PROJECT_NAME=$CI_PROJECT_NAME

RUN yarn build
RUN rm dist/js/*.map
RUN find ./dist -type f -regex '.*\.\(htm\|html\|txt\|text\|js\|css\)$' -exec gzip -f -k {} \;

# Serve static content
FROM nginx:1.21.6 as production-stage
COPY --from=node-build-stage /app/tart-vuer/dist /usr/share/nginx/html
EXPOSE 80
