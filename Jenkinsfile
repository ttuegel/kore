pipeline {
    agent any
    stages {
        stage('Build - Java') {
            agent {
                docker {
                    image 'maven:3-jdk-8'
                    args '-v $HOME/.m2:/root/.m2'
                }
            }
            steps {
                sh '''
                    env
                    ls -la /root/.m2
                    mvn clean
                    mvn verify
                '''
            }
        }
        stage('Build - Haskell') {
            agent { docker { image 'nixos/nix' } }
            steps {
                sh '''
                    export HOME=/root
                    ls -la
                    nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
                    nix-channel --update
                    export STACK_OPTS='--test --bench --coverage'
                    nix run nixpkgs.stack -c make test-kore
                '''
            }
        }
    }
}