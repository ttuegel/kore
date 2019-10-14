module Main (main) where

import Control.Monad.Catch
import Control.Monad.Reader
import Data.Foldable
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Vector (Vector)

import qualified GitHub
import qualified GitHub.Endpoints.PullRequests as GitHub

import System.Environment (lookupEnv)

getAuth :: IO GitHub.Auth
getAuth = do
    token <- fromMaybe unauthorized <$> lookupEnv "GITHUB_TOKEN"
    return $ GitHub.OAuth $ fromString token
  where
    unauthorized = error "Please set GITHUB_TOKEN"

data App =
    App
        { auth :: GitHub.Auth
        , owner :: GitHub.Name GitHub.Owner
        , repo :: GitHub.Name GitHub.Repo
        }

pullRequests :: ReaderT App IO (Vector GitHub.SimplePullRequest)
pullRequests = do
    App { auth, owner, repo } <- ask
    liftIO do
        GitHub.pullRequestsFor' (Just auth) owner repo
            >>= either throwM return

pullRequestReviews
    :: GitHub.IssueNumber
    -> ReaderT App IO (Vector GitHub.Review)
pullRequestReviews issueNumber = do
    App { auth, owner, repo } <- ask
    liftIO do
        GitHub.pullRequestReviews' (Just auth) owner repo issueNumber
            >>= either throwM return

main :: IO ()
main = do
    auth <- getAuth
    runApp auth do
        pulls <- pullRequests
        for_ pulls \pull -> do
            let number = GitHub.simplePullRequestNumber pull
            reviews <- pullRequestReviews number
            when (hasApproval reviews) do
                liftIO do print number
  where
    runApp auth go =
        runReaderT go App { auth, owner = "kframework", repo = "kore" }

hasApproval :: Foldable f => f GitHub.Review -> Bool
hasApproval reviews =
    any isApproved reviews && all (not . isChangesRequested) reviews
  where
    isApproved GitHub.Review { reviewState } =
        reviewState == GitHub.ReviewStateApproved
    isChangesRequested GitHub.Review { reviewState } =
        reviewState == GitHub.ReviewStateChangesRequested
