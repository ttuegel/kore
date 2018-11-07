pipeline {
    agent any
    stages {
        stage('Build - Java') {
            agent { docker { image 'maven:3-jdk-8' } }
            steps {
                sh '''
                    mvn clean
                    mvn verify
                '''
            }
        }
        stage('Build - Haskell') {
            agent { docker { image 'nixorg/nix:latest' } }
            steps {
                sh '''
                    export STACK_OPTS='--test --bench --coverage'
                    nix run -f channel:nixos-18.09 nixpkgs.stack \
                        -c make test-kore
                '''
            }
        }
    }
}