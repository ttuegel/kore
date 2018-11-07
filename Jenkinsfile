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
            agent { docker { image 'haskell:8.4' } }
            environment {
                STACK_ROOT = '/tmp/stack_root'
            }
            steps {
                sh '''
                    mkdir -p "$STACK_ROOT"
                    export STACK_OPTS='--test --bench --coverage'
                    make test-kore
                '''
            }
        }
    }
}