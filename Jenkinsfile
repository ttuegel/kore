pipeline {
    agent { dockerfile true }
    stages {
        stage('Build - Java') {
            steps {
                sh '''
                    mvn clean
                    mvn verify
                '''
            }
        }
        stage('Build - Haskell') {
            environment {
                STACK_ROOT = '/tmp/stack_root'
            }
            steps {
                sh '''
                    mkdir -p "$STACK_ROOT"
                    stack setup --verbose
                    export STACK_OPTS='--nix --test --bench --coverage'
                    nix run -f channel:nixos-18.09 stack -c make test-kore
                '''
            }
        }
    }
}