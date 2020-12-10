export UID=$(id -u)
export GID=$(id -g)
alias darn="docker run -it -u $(id -u):$(id -g) -w /opt/app -p '5000:5000' -v $(pwd):/opt/app -v /etc/group:/etc/group:ro -v /etc/passwd:/etc/passwd:ro -v /etc/shadow:/etc/shadow:ro garg-frontend-dev:latest yarn"
alias duild="docker build . -f Dockerfile.dev -t garg-frontend-dev:latest"
