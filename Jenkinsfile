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
                    locale -a
                    mkdir -p "$STACK_ROOT"
                    stack setup --verbose
                    export STACK_OPTS='--test --bench --coverage'
                    make test-kore
                '''
            }
        }
    }
}