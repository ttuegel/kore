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
            agent { docker { image 'fretlink/nix' } }
            environment {
                STACK_ROOT = '/tmp/stack_root'
            }
            steps {
                sh '''
                    mkdir -p "$STACK_ROOT"
                    export STACK_OPTS='--nix --test --bench --coverage'
                    nix run -f channel:nixos-18.09 stack -c make test-kore
                '''
            }
        }
    }
}