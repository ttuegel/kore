pipeline {
    agent any
    stages {
        stage('Build - Java') {
            agent { docker { image 'maven:3' } }
            steps {
                sh '''
                    mvn clean
                    mvn verify
                '''
            }
        }
    }
}