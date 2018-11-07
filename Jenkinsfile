pipeline {
    agent any
    stages {
        stage('Build - Java') {
            agent { docker { image 'maven:3-jdk-8' } }
            steps {
                sh '''
                    env
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
                    nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs
                    nix-channel --update
                    export STACK_OPTS='--test --bench --coverage'
                    nix run nixpkgs.stack -c make test-kore
                '''
            }
        }
    }
}