pipeline {
    agent {
        dockerfile true
    }
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
                    export STACK_OPTS='--test --bench --coverage'
                    make test-kore
                '''
            }
        }
    }
}